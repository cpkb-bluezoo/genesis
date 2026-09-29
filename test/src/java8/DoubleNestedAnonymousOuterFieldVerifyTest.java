/*
 * Regression test: a class nested TWO levels deep inside anonymous classes
 * (anonymous class B inside anonymous class A, both inside some outer real
 * class Outer) that implicitly references a field declared on the
 * OUTERMOST class Outer (no explicit qualifier, e.g. no "Outer.this.") used
 * to generate WRONG bytecode: only ONE "getfield this$0"-style hop was
 * emitted, when TWO are needed to actually reach Outer from the doubly-
 * nested class. At runtime this produced:
 *
 *   java.lang.VerifyError: Bad type on operand stack
 *   Reason: Type 'Outer$1' (current frame, stack[0]) is not assignable to
 *   'Outer'
 *
 * Matches the shape found in gumdrop's own
 * RecoverableConnectionImpl.channelOpen(): an anonymous
 * ChannelOpenHandler's handleChannelOpenOk() method creates an anonymous
 * Runnable whose run() body implicitly references a field (and a captured
 * parameter) declared on the outermost enclosing class, two anonymous-class
 * levels away.
 *
 * Root cause: codegen_expr.c had three independent single-hop shortcuts,
 * each hardcoding exactly one "aload_0; getfield this$0" using the CURRENT
 * class's own this$0 fieldref, then treating the result as if it already
 * were the field's declaring (possibly much further out) enclosing class -
 * regardless of how many levels of anonymous/inner/local-class nesting
 * actually separate the two:
 *   1. codegen_identifier()'s plain field-read path (~line 1298).
 *   2. codegen_assignment()'s plain-assignment/compound-assignment path
 *      (~line 7940), covering both e.g. "channels.put(...)" style member
 *      calls (which read the field first) and "counter += 1;"/"counter =
 *      ...;" directly.
 *   3. codegen_expr()'s ++/-- path (~line 9232), covering "counter++;" /
 *      "counter--;".
 * genesis already had correct, general multi-hop "walk the this$0 chain"
 * logic elsewhere in the same file (codegen_load_enclosing_this(), used by
 * "outer.new Inner()" and by an unqualified call to an enclosing class's
 * instance method) - fixed by having all three call sites above reuse that
 * shared helper instead of their own hardcoded single hop.
 *
 * This test exercises all three call sites from a class nested two levels
 * deep in anonymous classes, referencing a field (read, compound-assign,
 * increment) and a captured local declared on the outermost class.
 */
public class DoubleNestedAnonymousOuterFieldVerifyTest {
    private int counter = 0;
    private final java.util.Map<Integer, String> channels = new java.util.HashMap<>();

    interface Handler {
        void handle(int id);
    }

    void register(final int channelId, Handler outer) {
        outer.handle(channelId);
        Runnable inner = new Runnable() {
            @Override
            public void run() {
                // Anonymous class nested inside this anonymous Runnable -
                // two levels of anonymous nesting away from the outermost
                // DoubleNestedAnonymousOuterFieldVerifyTest instance.
                new Handler() {
                    @Override
                    public void handle(int id) {
                        /* Plain field read (implicit receiver), two levels
                         * deep - exercises codegen_identifier()'s read path. */
                        channels.remove(channelId);
                        channels.put(channelId, "done");

                        /* Compound assignment to the outer field, two
                         * levels deep - exercises codegen_assignment(). */
                        counter += 1;

                        /* Increment of the outer field, two levels deep -
                         * exercises the ++/-- path. */
                        counter++;
                    }
                }.handle(channelId);
            }
        };
        inner.run();
    }

    public static void main(String[] args) {
        DoubleNestedAnonymousOuterFieldVerifyTest t =
            new DoubleNestedAnonymousOuterFieldVerifyTest();
        t.channels.put(1, "x");
        t.register(1, new Handler() {
            @Override
            public void handle(int id) {
                /* Single-level anonymous class - already worked before
                 * this fix; kept here to match gumdrop's exact shape. */
            }
        });

        if (!"done".equals(t.channels.get(1))) {
            throw new RuntimeException("expected channels to contain 'done', got: " + t.channels);
        }
        if (t.counter != 2) {
            throw new RuntimeException("expected counter == 2, got: " + t.counter);
        }
        System.out.println("OK");
    }
}
