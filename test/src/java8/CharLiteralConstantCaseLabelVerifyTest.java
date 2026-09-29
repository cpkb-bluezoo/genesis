/*
 * Regression test: a byte/char constant initialized with a CHAR literal
 * (e.g. "private static final byte TAG_A = 'a';", exactly gumdrop's own
 * AMQP FieldTable tag constants: "TAG_BOOLEAN = 't'", "TAG_VOID = 'V'",
 * etc.) used as a switch case label used to compile silently but
 * generate a broken lookupswitch with every case's match key read as 0:
 *
 *   java.lang.VerifyError: Bad lookupswitch instruction
 *
 * Root cause: the switch-statement case-label processing in semantic.c
 * (both the bare-identifier branch and the qualified Type.CONSTANT
 * branch) only ever decoded a case label constant's own literal
 * initializer when it was a TOK_INTEGER_LITERAL, deliberately skipping
 * TOK_CHAR_LITERAL (a char literal's value is stored as a one-character
 * string in the leaf's str_val, not in int_val, so reading int_val
 * directly would read the wrong union member) - every char-literal-
 * initialized constant case label therefore kept its default int_val
 * of 0 all the way to codegen. Fixed by decoding TOK_CHAR_LITERAL the
 * same way this file's own narrowing_constant_allowed() helper already
 * does elsewhere: the character is the first byte of str_val.
 */
public class CharLiteralConstantCaseLabelVerifyTest {
    private static final byte TAG_A = 'a';
    private static final byte TAG_B = 'b';
    private static final byte TAG_C = 'c';

    static String dispatch(byte tag) {
        switch (tag) {
            case TAG_A:
                return "A";
            case TAG_B:
                return "B";
            case TAG_C:
                return "C";
            default:
                return "unknown";
        }
    }

    public static void main(String[] args) {
        String result = dispatch((byte) 'a') + "," + dispatch((byte) 'b') + ","
                + dispatch((byte) 'c') + "," + dispatch((byte) 'z');
        if (!"A,B,C,unknown".equals(result)) {
            throw new RuntimeException("expected A,B,C,unknown but got " + result);
        }
        System.out.println("CharLiteralConstantCaseLabelVerifyTest passed!");
    }
}
