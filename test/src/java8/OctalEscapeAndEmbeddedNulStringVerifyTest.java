import java.nio.charset.StandardCharsets;
import java.util.Arrays;

/*
 * Regression test for two stacked bugs, both needed to correctly compile
 * a string literal using a JLS 3.10.6 octal escape (\0 through \377) -
 * found while chasing gumdrop's
 * Amqp1ClientProtocolHandlerTest.testPlainSendsInitialResponse, which
 * asserts against "\0alice\0s3cret" (a SASL PLAIN initial response).
 *
 * Bug 1 (lexer.c, lexer_scan_string): the STRING literal escape-sequence
 * switch had no case at all for an octal digit - it fell through to
 * "default: esc = c;", which kept the literal DIGIT CHARACTER itself
 * (e.g. '0', byte 48) instead of the octal value it denotes (e.g. NUL,
 * byte 0). lexer_scan_char (for 'c' CHAR literals) already had the
 * correct logic; this test's octalEscapeValues() covers the same shape
 * for STRING literals, including the "how many digits does an escape
 * consume" edge cases: \0 through \7 (1 digit), \00 through \77 (2
 * digits, unless the following character isn't an octal digit - e.g.
 * "\08", where JLS says \0 is NUL followed by the ordinary character
 * '8', since '8' isn't a valid octal digit), and \000 through \377 (3
 * digits, first digit restricted to 0-3).
 *
 * Bug 2 (parser.c ast_new_literal_from_lexer, plus its downstream
 * consumers in constpool.c/classwriter.c/codegen_expr.c): once the
 * lexer correctly produced a genuine embedded NUL byte, the AST layer's
 * ast_new_literal_from_lexer() copied the lexer's scanned text into the
 * literal's str_val via strdup() - which stops at the first NUL byte,
 * silently truncating any string literal with an embedded NUL down to
 * just the part before it (a leading "\0", as in "\0alice\0s3cret", was
 * truncated to nothing at all - an empty string). Fixed by threading the
 * lexer's own tracked length (not strlen()) through a new str_len field
 * on the AST leaf, through a new length-aware cp_add_utf8_len()/
 * cp_add_string_len() constant-pool API (bypassing the UTF8 dedup
 * cache's own embedded-NUL blind spot for such a value), to the actual
 * classfile writer - which now also correctly emits an embedded NUL as
 * the two-byte "modified UTF-8" sequence 0xC0 0x80 required by JVMS
 * 4.4.7, rather than either truncating it or (had length alone been
 * fixed without this) writing a raw, illegal 0x00 byte.
 */
public class OctalEscapeAndEmbeddedNulStringVerifyTest {

    public static void main(String[] args) {
        octalEscapeValues();
        embeddedNulAtStart();
        embeddedNulInMiddleWithContentAfter();
        octalEscapeDoesNotOverconsumeDigits();
    }

    private static void octalEscapeValues() {
        char a = '\101';          /* 3-digit octal, first digit <= 3 */
        if (a != 'A') {
            throw new RuntimeException("\\101 should be 'A', got: " + (int) a);
        }
        char nul = '\0';
        if (nul != 0) {
            throw new RuntimeException("\\0 should be NUL, got: " + (int) nul);
        }
        char max = '\377';        /* largest legal octal escape, 0xFF */
        if (max != 0xFF) {
            throw new RuntimeException("\\377 should be 0xFF, got: " + (int) max);
        }
        String abc = "\101\102\103";
        if (!"ABC".equals(abc)) {
            throw new RuntimeException("\\101\\102\\103 should be \"ABC\", got: " + abc);
        }
    }

    private static void embeddedNulAtStart() {
        String s = "\0alice";
        if (s.length() != 6) {
            throw new RuntimeException("expected length 6, got " + s.length());
        }
        byte[] b = s.getBytes(StandardCharsets.UTF_8);
        byte[] expected = { 0, 'a', 'l', 'i', 'c', 'e' };
        if (!Arrays.equals(expected, b)) {
            throw new RuntimeException("embeddedNulAtStart mismatch: " + Arrays.toString(b));
        }
    }

    private static void embeddedNulInMiddleWithContentAfter() {
        /* Matches gumdrop's own SASL PLAIN shape exactly:
         * "\0" + authzid-less-user + "\0" + password. */
        String s = "\0alice\0s3cret";
        if (s.length() != 13) {
            throw new RuntimeException("expected length 13, got " + s.length());
        }
        byte[] b = s.getBytes(StandardCharsets.UTF_8);
        byte[] expected = {
            0, 'a', 'l', 'i', 'c', 'e', 0, 's', '3', 'c', 'r', 'e', 't'
        };
        if (!Arrays.equals(expected, b)) {
            throw new RuntimeException("embeddedNulInMiddleWithContentAfter mismatch: "
                                        + Arrays.toString(b));
        }
        if (b.length != 13) {
            throw new RuntimeException("expected 13 bytes, got " + b.length);
        }
    }

    private static void octalEscapeDoesNotOverconsumeDigits() {
        /* "\0" followed by the ordinary character '8' - '8' is not a
         * valid octal digit, so the escape must consume only the '0'. */
        String s = "\08";
        if (s.length() != 2) {
            throw new RuntimeException("expected length 2, got " + s.length());
        }
        if (s.charAt(0) != 0 || s.charAt(1) != '8') {
            throw new RuntimeException("\\08 should be NUL,'8', got: "
                                        + (int) s.charAt(0) + "," + (int) s.charAt(1));
        }
    }
}
