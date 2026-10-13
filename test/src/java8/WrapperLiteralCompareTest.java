/* Regression test: "==" and "!=" between a wrapper and a char or boolean
 * literal compare the unboxed values (JLS 15.21). The literal check covered the
 * numeric literal tokens only, so "Character == 'b'" was compiled as a
 * reference comparison (VerifyError). */
public class WrapperLiteralCompareTest {
    private static void check(boolean ok, String what) {
        if (!ok) {
            System.out.println("FAILED: " + what);
            System.exit(1);
        }
    }

    public static void main(String[] args) {
        Character c = Character.valueOf('b');
        Boolean yes = Boolean.TRUE;
        Boolean no = Boolean.FALSE;
        check(c == 'b', "Character == char literal");
        check('b' == c, "char literal == Character");
        check(c != 'x', "Character != char literal");
        check(!('x' == c), "char literal == Character, false");
        check(yes == true, "Boolean == true");
        check(true == yes, "true == Boolean");
        check(no == false, "Boolean == false");
        check(no != true, "Boolean != true");
        Character[] cs = {'q'};
        check(cs[0] == 'q', "Character element == literal");
        System.out.println("WrapperLiteralCompareTest passed!");
    }
}
