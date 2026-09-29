/*
 * Regression test: a Unicode escape sequence (JLS 3.3, backslash + 'u' +
 * four hex digits) embedded inside a STRING literal - matching gumdrop's
 * own SaslUtils.parseOAuthBearerCredentials(), which uses control-A
 * (0x01) as the RFC 7628 OAUTHBEARER field separator.
 *
 * Root cause: lexer.c's lexer_scan_string() had no 'u' case in its
 * escape-sequence switch - unlike lexer_scan_char(), which already
 * handled this escape correctly for CHAR literals. Inside a string
 * literal, the escape fell through to the switch's default case, which
 * kept the literal 'u' character and left the following four hex digits
 * as ordinary text characters - inflating the string by 4 extra bytes
 * per escape (5 bytes emitted instead of the single intended byte)
 * instead of decoding it to the single byte it names. Every
 * indexOf(char) call against such a corrupted string literal, searching
 * for the correctly-decoded CHAR literal, then failed to find any match
 * at all.
 *
 * Fixed by adding the same escape handling already used by
 * lexer_scan_char() to lexer_scan_string()'s escape switch.
 */
public class UnicodeEscapeInStringLiteralVerifyTest {
    public static void main(String[] args) {
        String s = "a\u0001b\u0001c";

        if (s.length() != 5) {
            throw new RuntimeException("expected length 5, got " + s.length()
                    + " (\"" + s + "\")");
        }

        int first = s.indexOf('\u0001');
        if (first != 1) {
            throw new RuntimeException("expected first control-A at index 1, got " + first);
        }

        int second = s.indexOf('\u0001', first + 1);
        if (second != 3) {
            throw new RuntimeException("expected second control-A at index 3, got " + second);
        }

        String[] parts = s.split("\u0001");
        if (parts.length != 3 || !"a".equals(parts[0]) || !"b".equals(parts[1]) || !"c".equals(parts[2])) {
            StringBuilder sb = new StringBuilder();
            for (String p : parts) {
                sb.append("[").append(p).append("]");
            }
            throw new RuntimeException("expected [a][b][c], got " + sb + " (length " + parts.length + ")");
        }

        /* A hex escape must consume exactly 4 hex digits, not more - the
         * escape for 'A' (0x0041) followed by the literal digit '1' must
         * produce "A1", not read a 5th hex digit. */
        String hexBoundary = "A1";
        if (hexBoundary.length() != 2 || hexBoundary.charAt(0) != 'A' || hexBoundary.charAt(1) != '1') {
            throw new RuntimeException("expected \"A1\", got \"" + hexBoundary + "\"");
        }

        System.out.println("UnicodeEscapeInStringLiteralVerifyTest passed!");
    }
}
