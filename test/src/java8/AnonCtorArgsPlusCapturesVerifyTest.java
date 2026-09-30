/*
 * An anonymous class instantiated with BOTH explicit superclass
 * constructor arguments ("new Base(\"hello\") { ... }") AND captured
 * effectively-final local variables needs a synthesized constructor
 * whose parameter layout is consistent between (a) the constructor's own
 * descriptor/slot accounting and (b) the call site that invokes it.
 * codegen_anonymous_class() (codegen.c) built its descriptor and slot
 * accounting as [outer instance, explicit super-ctor args, captures],
 * while the AST_NEW_OBJECT call site (codegen_expr.c) pushes arguments
 * as [outer instance, captures, explicit super-ctor args] (matching real
 * javac's own synthesized-constructor parameter order) - self-consistent
 * on each side alone, but disagreeing with each other. Depending on
 * which mismatch surfaced first this produced either a
 * "NoSuchMethodError" (wrong descriptor at the call site) or - as seen
 * here - a "Bad (local variable) type" VerifyError inside the
 * constructor itself, reading a captured int/array from the slot the
 * explicit constructor argument actually occupies (or vice versa).
 *
 * Confirmed against gumdrop's own DefaultServletCopyBufferTest, whose
 * "new FilterInputStream(new ByteArrayInputStream(payload)) { ... }"
 * anonymous subclass captures both a local int and a local int[]
 * alongside the explicit ByteArrayInputStream constructor argument.
 */
public class AnonCtorArgsPlusCapturesVerifyTest {
    static class Base {
        final String label;
        Base(String label) {
            this.label = label;
        }
        int compute() {
            return 0;
        }
    }

    public static void main(String[] args) {
        final int payloadSize = 4;
        final int[] largestRead = { 0 };
        Base b = new Base("hello") {
            @Override
            int compute() {
                largestRead[0] = payloadSize;
                return payloadSize + label.length();
            }
        };
        int result = b.compute();
        if (result != 9 || largestRead[0] != 4) {
            throw new RuntimeException("expected 9/4, got " + result + "/" + largestRead[0]);
        }
        System.out.println("AnonCtorArgsPlusCapturesVerifyTest passed!");
    }
}
