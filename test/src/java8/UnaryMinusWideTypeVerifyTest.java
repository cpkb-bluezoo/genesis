/*
 * Regression test: unary minus ("-x") on a long, float, or double value
 * always emitted the int-only INEG opcode, regardless of the operand's
 * actual type - genesis has no constant-folding for negative numeric
 * literals at parse time, so even a simple literal like "-1L" is parsed
 * as AST_UNARY_EXPR(TOK_MINUS, AST_LITERAL(long 1)) and went through this
 * same buggy codegen path. Applying INEG to a 2-slot long/double value (or
 * a float value, which INEG's "integer" typing rejects outright) corrupts
 * the operand stack, e.g.:
 *
 *   java.lang.VerifyError: Bad type on operand stack
 *   Reason: Type long_2nd (current frame, stack[1]) is not assignable to
 *   integer
 *
 * matching gumdrop's own Amqp1TypesTest.testSignedIntegers(), which calls
 * roundTrip(Long.valueOf(-1L)).
 *
 * Fixed by switching on the operand's real type (via the existing
 * get_expr_type_kind() helper, already used elsewhere for arithmetic
 * opcode selection) to pick LNEG/FNEG/DNEG/INEG.
 */
public class UnaryMinusWideTypeVerifyTest {
    static long negLong(long x) { return -x; }
    static float negFloat(float x) { return -x; }
    static double negDouble(double x) { return -x; }

    public static void main(String[] args) {
        long l = -1L;
        if (l != -1L) {
            throw new RuntimeException("expected -1, got " + l);
        }

        Long boxed = Long.valueOf(-1L);
        if (boxed.longValue() != -1L) {
            throw new RuntimeException("expected -1, got " + boxed);
        }

        if (negLong(5L) != -5L) {
            throw new RuntimeException("expected -5, got " + negLong(5L));
        }

        float f = -2.5f;
        if (f != -2.5f) {
            throw new RuntimeException("expected -2.5, got " + f);
        }
        if (negFloat(3.25f) != -3.25f) {
            throw new RuntimeException("expected -3.25, got " + negFloat(3.25f));
        }

        double d = -1.5;
        if (d != -1.5) {
            throw new RuntimeException("expected -1.5, got " + d);
        }
        if (negDouble(7.5) != -7.5) {
            throw new RuntimeException("expected -7.5, got " + negDouble(7.5));
        }

        System.out.println("UnaryMinusWideTypeVerifyTest passed!");
    }
}
