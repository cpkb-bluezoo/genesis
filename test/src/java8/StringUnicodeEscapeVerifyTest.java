/**
 * Bug: a `\uXXXX` unicode escape (JLS 3.3) inside a STRING literal for a
 * code point >= 0x80 was truncated to a single raw byte
 * ((char)(value &amp; 0xFF)) instead of being UTF-8 encoded, corrupting the
 * classfile's UTF8 constant pool entry - genesis itself compiled and wrote
 * the class file without complaint, but the JVM's own class-file verifier
 * rejected it at class-LOAD time with "java.lang.ClassFormatError: Illegal
 * UTF8 string in constant pool", for any string literal using such an
 * escape (Latin-1 supplement and above). A code point &lt;= 0x7F was
 * unaffected (one byte either way). Confirmed against gumdrop's own
 * HTTPUtilsTest, whose "caf\u00E9".equals(...) assertion depends on
 * exactly this.
 *
 * Expected values below are written as plain int comparisons rather than
 * char literals above 0xFF - a \uXXXX escape inside a CHAR literal (as
 * opposed to a STRING literal, which is what this bug is about) has its
 * own separate, pre-existing, already-documented "for now" limitation
 * (lexer_scan_char() truncates any code point above 0xFF), not exercised
 * or fixed here.
 */
public class StringUnicodeEscapeVerifyTest {
    public static void main(String[] args) {
        String twoByte = "caf\u00E9";
        if (twoByte.length() != 4) {
            throw new RuntimeException("expected length 4, got " + twoByte.length());
        }
        if ((int) twoByte.charAt(3) != 0x00E9) {
            throw new RuntimeException("expected char[3]==0x00E9, got " +
                Integer.toHexString(twoByte.charAt(3)));
        }

        String threeByte = "yen\u00A5euro\u20ACdone";
        if (threeByte.length() != 13) {
            throw new RuntimeException("expected length 13, got " + threeByte.length());
        }
        if ((int) threeByte.charAt(3) != 0x00A5) {
            throw new RuntimeException("expected char[3]==0x00A5, got " +
                Integer.toHexString(threeByte.charAt(3)));
        }
        if ((int) threeByte.charAt(8) != 0x20AC) {
            throw new RuntimeException("expected char[8]==0x20AC, got " +
                Integer.toHexString(threeByte.charAt(8)));
        }

        String ascii = "ctrl\u0001end";
        if (ascii.length() != 8 || ascii.charAt(4) != 1) {
            throw new RuntimeException("expected an unaffected ASCII-range escape to still work: length=" +
                ascii.length() + " charAt(4)=" + (int) ascii.charAt(4));
        }
    }
}
