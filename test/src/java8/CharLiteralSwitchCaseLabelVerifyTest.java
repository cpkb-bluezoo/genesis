/*
 * Regression test: a `switch` statement whose case labels are CHAR
 * LITERALS directly (e.g. "case '*':"), not identifiers or qualified
 * constants - matching gumdrop's own LdapRealm.escapeLDAPFilter(), whose
 * switch(char) dispatches on '\\', '*', '(', ')', and '\u0000'.
 *
 * Root cause: an AST literal node stores a CHAR literal's value in its
 * "value.str_val" union member (a 1-byte string, e.g. "a") - unlike
 * every other numeric literal kind (int/long store into "value.int_val");
 * see ast_new_literal_from_lexer() in parser.c, which never populates
 * int_val for TOK_CHAR_LITERAL. codegen_stmt.c's AST_SWITCH_STMT case-
 * value collection unconditionally read case_expr->data.leaf.value.int_val
 * for every AST_LITERAL case label, regardless of its token type - for a
 * char literal, that read the str_val POINTER's own bit pattern
 * reinterpreted as an int instead of the character's code point, a
 * wildly wrong lookupswitch key that could never match any actual switch
 * value at runtime. Every char-literal case label therefore silently
 * routed to "default" no matter what value was actually switched on.
 *
 * Fixed by special-casing TOK_CHAR_LITERAL to read the character's code
 * point from str_val[0] instead of int_val.
 */
public class CharLiteralSwitchCaseLabelVerifyTest {
    static String escape(String value) {
        StringBuilder sb = new StringBuilder();
        for (char c : value.toCharArray()) {
            switch (c) {
                case '\\':
                    sb.append("\\5c");
                    break;
                case '*':
                    sb.append("\\2a");
                    break;
                case '(':
                    sb.append("\\28");
                    break;
                case ')':
                    sb.append("\\29");
                    break;
                case '\u0000':
                    sb.append("\\00");
                    break;
                default:
                    sb.append(c);
            }
        }
        return sb.toString();
    }

    public static void main(String[] args) {
        String result = escape("a*(b)\\c" + (char) 0);
        String expected = "a\\2a\\28b\\29\\5cc\\00";
        if (!expected.equals(result)) {
            throw new RuntimeException("expected [" + expected + "], got [" + result + "]");
        }

        /* A plain char (not escaped/punctuation) case label must also
         * match, not just fall through to default. */
        char letter = 'a';
        String matched;
        switch (letter) {
            case 'a':
                matched = "matched-a";
                break;
            default:
                matched = "default:" + (int) letter;
        }
        if (!"matched-a".equals(matched)) {
            throw new RuntimeException("expected matched-a, got " + matched);
        }

        System.out.println("CharLiteralSwitchCaseLabelVerifyTest passed!");
    }
}
