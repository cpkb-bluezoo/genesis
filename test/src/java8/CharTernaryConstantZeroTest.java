/* Regression test: JLS 15.25 - a conditional with a char operand and an int
 * constant representable in char has type char, so it can initialize a char
 * without a cast ("char b = i < n ? s.charAt(i) : 0"). Mirrors gumdrop's
 * MessageDateTimeFormatter.zoneName. */
public class CharTernaryConstantZeroTest {
    static int sum(String s, int from, int to) {
        char a = s.charAt(from);
        char b = from + 1 < to ? s.charAt(from + 1) : 0;
        char c = from + 2 < to ? 0 : s.charAt(from);
        byte d = from > 100 ? s.getBytes()[0] : 7;
        short e = from > 100 ? (short) 1 : 300;
        return a + b + c + d + e;
    }

    public static void main(String[] args) {
        // from=0,to=2: b='y', c='x'
        int want1 = 'x' + 'y' + 'x' + 7 + 300;
        if (sum("xyz", 0, 2) != want1) {
            throw new RuntimeException("got " + sum("xyz", 0, 2) + " want " + want1);
        }
        // from=0,to=3: b='y', c=0
        int want2 = 'x' + 'y' + 0 + 7 + 300;
        if (sum("xyz", 0, 3) != want2) {
            throw new RuntimeException("got " + sum("xyz", 0, 3) + " want " + want2);
        }
        // from=0,to=1: b=0, c='x'
        int want3 = 'x' + 0 + 'x' + 7 + 300;
        if (sum("xyz", 0, 1) != want3) {
            throw new RuntimeException("got " + sum("xyz", 0, 1) + " want " + want3);
        }
        System.out.println("CharTernaryConstantZeroTest passed!");
    }
}
