/**
 * Bug: a supplementary-plane character (U+10000-U+10FFFF, e.g. an emoji)
 * written directly in a string literal's SOURCE TEXT (not via a \uXXXX
 * escape, which can only ever name a single BMP code point) was copied
 * through byte-for-byte as its raw 4-byte UTF-8 encoding - valid standard
 * UTF-8, but the classfile format's "modified UTF-8" explicitly forbids
 * 4-byte sequences, requiring the code point's UTF-16 surrogate pair
 * instead, each half encoded as its own ordinary 3-byte sequence. Genesis
 * itself compiled and wrote the class file without complaint; only the
 * JVM's own class-LOAD-time verifier caught it, with
 * "java.lang.ClassFormatError: Illegal UTF8 string in constant pool".
 * Confirmed against gumdrop's own MessageIndexEntryTest, whose test data
 * includes a literal emoji written directly in the source.
 */
public class AstralCharacterStringVerifyTest {
    public static void main(String[] args) {
        String s = "Party invitation! 🎉🎂";
        if (s.length() != 22) {
            throw new RuntimeException("expected length 22, got " + s.length());
        }
        if (s.codePointCount(0, s.length()) != 20) {
            throw new RuntimeException("expected 20 code points, got " + s.codePointCount(0, s.length()));
        }
        int firstEmoji = s.codePointAt(18);
        if (firstEmoji != 0x1F389) {
            throw new RuntimeException("expected first emoji code point 0x1F389, got " +
                Integer.toHexString(firstEmoji));
        }
        int secondEmoji = s.codePointAt(20);
        if (secondEmoji != 0x1F382) {
            throw new RuntimeException("expected second emoji code point 0x1F382, got " +
                Integer.toHexString(secondEmoji));
        }
    }
}
