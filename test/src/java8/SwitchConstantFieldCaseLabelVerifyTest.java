/*
 * Regression test: a switch over a plain int (not an enum) whose case
 * labels are named "private static final int" constants (not literals)
 * used to produce a classfile that failed JVM bytecode verification:
 *
 *   java.lang.VerifyError: Bad lookupswitch instruction
 *   Reason: Error exists in the bytecode
 *
 * Decoding the actual lookupswitch showed every case's match value
 * encoded as 0, regardless of the named constant's real value - a
 * lookupswitch's match keys must be distinct and strictly ascending, so
 * four (or more) identical zero keys is invalid.
 *
 * codegen_stmt.c's case-value extraction only ever read a case label's
 * resolved int value from case_expr->data.leaf.value.int_val for two
 * shapes: a bare integer literal (case 4:), or - gated specifically on
 * is_enum_switch - an identifier resolved as an enum constant, whose
 * ordinal semantic.c's switch handling already stores there. For a case
 * label that's an identifier referring to a plain "static final int"
 * constant field in a *non*-enum switch, nothing ever resolved or stored
 * that constant's value anywhere - semantic.c's own handling of this
 * exact shape only ever *validated* that the referenced field was final
 * (for the "constant expression required" check), without extracting its
 * value - so the case label's leaf simply kept its default int_val of 0.
 *
 * Fixed by having semantic.c also resolve the field's own literal
 * initializer value and store it on the case label's leaf (mirroring the
 * enum-ordinal mechanism exactly), and having codegen_stmt.c read it for
 * an identifier case label regardless of whether the switch is over an
 * enum.
 */
public class SwitchConstantFieldCaseLabelVerifyTest {
    private static final int MODE_TOKEN = 0;
    private static final int MODE_RAW_FIXED = 1;
    private static final int MODE_RAW_UNTIL = 2;
    private static final int MODE_TEXT = 3;
    private static final int MODE_STOPPED = 4;

    static String describe(int mode) {
        switch (mode) {
            case MODE_STOPPED:
                return "stopped";
            case MODE_RAW_FIXED:
                return "raw_fixed";
            case MODE_RAW_UNTIL:
                return "raw_until";
            case MODE_TEXT:
                return "text";
            default:
                return "token";
        }
    }

    public static void main(String[] args) {
        String[] expected = { "token", "raw_fixed", "raw_until", "text", "stopped" };
        for (int i = 0; i < 5; i++) {
            String result = describe(i);
            if (!expected[i].equals(result)) {
                throw new RuntimeException("mode " + i + ": expected " + expected[i] + ", got " + result);
            }
        }
        System.out.println("All tests passed!");
    }
}
