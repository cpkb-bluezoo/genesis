/*
 * Regression test: a "double... values" varargs call mixing a double
 * literal with several int literals (JLS 5.1.2 numeric widening applies
 * per-argument) - matching gumdrop's own
 * DoubleHistogram.Builder.setExplicitBuckets(0.5, 1, 2, 5, 10, 25, 50,
 * 100, 250, 500, 1000). Used to throw at class-verification time:
 *
 *   java.lang.VerifyError: Bad type on operand stack
 *   Reason: Type integer (current frame, stack[N]) is not assignable to
 *   double
 *
 * Root cause (two separate gaps, both in the same varargs-array-building
 * loop in codegen_expr.c):
 *
 * 1. The loop's boxing check only handled building a REFERENCE varargs
 *    array (boxing a primitive argument for e.g. "Object... args"); a
 *    PRIMITIVE varargs array (e.g. "double... values") never widened a
 *    narrower primitive argument (e.g. an int literal) to the array's
 *    own declared element type at all, leaving a plain int on the stack
 *    where the subsequent DASTORE required a double.
 *
 * 2. Once every argument DOES land the correct (possibly wide) type on
 *    the stack, the loop's own stack-depth/stackmap bookkeeping
 *    (mg_pop_typed(mg, 3) for arrayref+index+value) assumed the value
 *    was always exactly one JVM word - correct for int/float/reference
 *    elements, but wrong for long/double (two words) - unlike the
 *    already-correct equivalent in ordinary (non-varargs) array-element
 *    assignment codegen, which pops an extra word first for exactly
 *    this case. Left unfixed, this drifted mg->stack_depth (and the
 *    mirrored stackmap word count) further out of sync with the real
 *    bytecode on every wide-typed varargs element, eventually tripping
 *    a later stack-depth-sensitive check.
 *
 * Fixed by widening via the existing coerce_stack_value() helper (used
 * elsewhere for the same purpose) for the primitive-array case, and by
 * adding the same "extra word" pop already used for ordinary array
 * assignment.
 */
public class VarargsDoubleWideningVerifyTest {
    static double sum(double... values) {
        double total = 0;
        for (double v : values) {
            total += v;
        }
        return total;
    }

    public static void main(String[] args) {
        double mixed = sum(0.5, 1, 2, 5, 10, 25, 50, 100, 250, 500, 1000);
        if (mixed != 1943.5) {
            throw new RuntimeException("expected 1943.5, got " + mixed);
        }

        double pure = sum(1.5, 2.5, 3.5);
        if (pure != 7.5) {
            throw new RuntimeException("expected 7.5, got " + pure);
        }

        System.out.println("VarargsDoubleWideningVerifyTest passed!");
    }
}
