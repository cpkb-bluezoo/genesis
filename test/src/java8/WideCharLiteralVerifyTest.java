/* Regression test: char literals outside ASCII - a raw character, a \uXXXX
 * escape, an octal escape above 127 - must keep their full UTF-16 code unit.
 * They were held in a single 8-bit char: '↵' became 0xB5, '\377' went
 * negative, and a raw '↵' wasn't scanned at all (the declaration holding it
 * vanished and the verifier rejected the method). Mirrors gumdrop's
 * ScriptletElement.toString(): preview.replace('\n', '↵'). */
public class WideCharLiteralVerifyTest {
    static final char ARROW = '↵';
    static final char RAW = '↵';
    static final char EURO = '€';
    static final char LATIN = 'é';
    static final char OCTAL = '\377';
    static final char NUL = '\0';

    static String kind(char c) {
        switch (c) {
            case '↵': return "arrow";
            case '€': return "euro";
            case 'é': return "latin";
            case '\377': return "octal";
            case 'a': return "a";
            default: return "other";
        }
    }

    public static void main(String[] args) {
        String p = "a\nb".replace('\n', '↵');
        if (p.length() != 3 || p.charAt(1) != 0x21b5) {
            throw new RuntimeException("replace: " + (int) p.charAt(1));
        }
        char[] all = { '↵', '↵', '€', 'é', '\377', '\0', 'z' };
        int[] want = { 0x21b5, 0x21b5, 0x20ac, 0xe9, 255, 0, 'z' };
        for (int i = 0; i < all.length; i++) {
            if (all[i] != want[i]) {
                throw new RuntimeException("literal " + i + ": " + (int) all[i]);
            }
        }
        if (ARROW != 0x21b5 || RAW != 0x21b5 || EURO != 0x20ac || LATIN != 0xe9
                || OCTAL != 255 || NUL != 0) {
            throw new RuntimeException("constants");
        }
        if (!"arrow".equals(kind('↵')) || !"euro".equals(kind('€')) || !"latin".equals(kind('é'))
                || !"octal".equals(kind('\377')) || !"a".equals(kind('a')) || !"other".equals(kind('b'))) {
            throw new RuntimeException("switch");
        }
        String s = "x" + '€' + 'é';
        if (s.length() != 3 || s.charAt(1) != 0x20ac || s.charAt(2) != 0xe9) {
            throw new RuntimeException("concat");
        }
        int sum = 0;
        for (char c = '↳'; c <= '↵'; c++) {
            sum++;
        }
        if (sum != 3) {
            throw new RuntimeException("loop " + sum);
        }
        System.out.println("WideCharLiteralVerifyTest passed!");
    }
}
