/*
 * Regression test: a `static final` field of a primitive/String type,
 * initialized with a literal, referenced from ANOTHER static field's own
 * initializer that is declared EARLIER in the same class - matching
 * gumdrop's own DnsServerCapabilityCache, whose "WELL_KNOWN =
 * wellKnownResolvers()" field (declared near the top of the class) calls
 * a method reading "DOH_PATH" (a static final String declared much later
 * in the same file). Used to silently produce a WRONG RUNTIME VALUE (no
 * VerifyError at all): the referenced constant read back as its type's
 * default value (null for a String) instead of its real one, because
 * genesis emitted a genuine runtime GETSTATIC for it, which observes the
 * class's own <clinit> field-initializer declaration order - the
 * constant's own assignment hadn't run yet at the point the earlier
 * field's initializer executed.
 *
 * Root cause: per JLS 4.12.4/13.1, a `static final` field of a
 * primitive or String type initialized with a compile-time-constant
 * expression (here, simply a literal) is itself a compile-time constant
 * - real javac inlines its value at every use site instead of ever
 * emitting a runtime field read for it, which is exactly what makes
 * real Java immune to this ordering hazard (there is no read to order
 * at all). codegen_expr.c's codegen_identifier() had no such inlining -
 * every static field reference, regardless of finality or its
 * initializer shape, went through a plain runtime GETSTATIC.
 *
 * Fixed by inlining the literal directly (via the existing
 * codegen_literal()) whenever the referenced field is `static final`
 * and its own declared initializer is a plain literal expression.
 *
 * That fix itself introduced a real regression, caught by this same
 * session's own next smoke run rather than by `make check` (no test
 * covered the shape): a `static final long` field declared with a plain
 * `int` literal initializer (e.g. "private static final long
 * DDR_TIMEOUT_MS = 3000;", legal per JLS 5.2's implicit widening at the
 * point of assignment - matching gumdrop's own
 * DnsResolver.DDR_TIMEOUT_MS, passed to an interface method's own `long`
 * parameter) got the bare `int` literal's own natural type inlined
 * verbatim, without the widening to the FIELD's actually-declared `long`
 * type that a real `getstatic` read would have gotten for free (a field
 * read's pushed type comes from the field's own descriptor, not its
 * initializer expression) - VerifyError: "Bad type on operand stack",
 * int not assignable to long_2nd. Fixed by widening the inlined
 * literal's value to the field's declared type when they differ,
 * exercised below by DELAY_MS/FACTOR/RATIO.
 */
public class StaticConstantForwardReferenceInClinitVerifyTest {
    private static final String COMPUTED = compute();
    static final String PATH = "/dns-query";

    private static String compute() {
        return "value=" + PATH;
    }

    interface Sink {
        void accept(long delayMs, double factor, float ratio);
    }

    private static final long DELAY_MS = 3000;
    private static final double FACTOR = 2;
    private static final float RATIO = 5;

    public static void main(String[] args) {
        if (!"value=/dns-query".equals(COMPUTED)) {
            throw new RuntimeException("expected value=/dns-query, got " + COMPUTED);
        }

        final long[] captured = new long[3];
        Sink sink = new Sink() {
            @Override
            public void accept(long delayMs, double factor, float ratio) {
                captured[0] = delayMs;
                captured[1] = (long) factor;
                captured[2] = (long) ratio;
            }
        };
        sink.accept(DELAY_MS, FACTOR, RATIO);
        if (captured[0] != 3000 || captured[1] != 2 || captured[2] != 5) {
            throw new RuntimeException("bad captured values: " + captured[0] + " "
                    + captured[1] + " " + captured[2]);
        }

        System.out.println("StaticConstantForwardReferenceInClinitVerifyTest passed!");
    }
}
