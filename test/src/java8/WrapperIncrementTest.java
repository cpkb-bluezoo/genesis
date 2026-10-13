/* Regression test: ++ and -- on a variable of a wrapper type (JLS 15.14,
 * 15.15) unbox, add one, convert back to the wrapper's type and store it, for a
 * local, a field and an array element, prefix and postfix, with the value used
 * or not. genesis compiled "Integer i = 1; i++;" to an int increment of a
 * reference local (VerifyError) and did not unbox a field's old value. */
public class WrapperIncrementTest {
    Byte b = 1;
    Short sh = 2;
    Character c = 'a';
    Integer i = 5;
    Long l = 6L;
    Float f = 1.5f;
    Double d = 2.5;
    static Integer si = 10;

    private static int failures;

    private static void check(Object expected, Object actual, String what) {
        if (!expected.equals(actual)) {
            System.out.println("FAILED " + what + ": expected " + expected + " (" + expected.getClass().getSimpleName()
                + "), got " + actual + " (" + actual.getClass().getSimpleName() + ")");
            failures++;
        }
    }

    static void locals() {
        Byte b = 1;
        b++;
        check((byte) 2, b, "Byte local ++");
        Short s = 2;
        s--;
        check((short) 1, s, "Short local --");
        Character c = 'a';
        ++c;
        check('b', c, "Character local prefix ++");
        Integer i = 5;
        i++;
        check(6, i, "Integer local ++");
        Long l = 6L;
        l--;
        check(5L, l, "Long local --");
        Float f = 1.5f;
        f++;
        check(2.5f, f, "Float local ++");
        Double d = 2.5;
        --d;
        check(1.5, d, "Double local prefix --");

        Integer n = 7;
        int old = n++;
        check(7, old, "postfix value of an Integer local");
        check(8, n, "Integer local after postfix");
        int now = ++n;
        check(9, now, "prefix value of an Integer local");
        Byte bb = 127;
        bb++;
        check((byte) -128, bb, "Byte local wraps");
        Character cc = 'z';
        char oldc = cc++;
        check('z', oldc, "postfix value of a Character local");
        check('{', cc, "Character local after postfix");
    }

    static void fields() {
        WrapperIncrementTest o = new WrapperIncrementTest();
        o.b++;
        check((byte) 2, o.b, "Byte field ++");
        o.sh--;
        check((short) 1, o.sh, "Short field --");
        ++o.c;
        check('b', o.c, "Character field prefix ++");
        o.i++;
        check(6, o.i, "Integer field ++");
        o.l--;
        check(5L, o.l, "Long field --");
        o.f++;
        check(2.5f, o.f, "Float field ++");
        --o.d;
        check(1.5, o.d, "Double field prefix --");
        int old = o.i++;
        check(6, old, "postfix value of an Integer field");
        check(7, o.i, "Integer field after postfix");
        int now = ++o.i;
        check(8, now, "prefix value of an Integer field");
        byte oldb = o.b++;
        check((byte) 2, oldb, "postfix value of a Byte field");
        si++;
        check(11, si, "static Integer field ++");
        int sold = si--;
        check(11, sold, "postfix value of a static Integer field");
        check(10, si, "static Integer field after --");
        o.i += 3;
        check(11, o.i, "Integer field +=");
        o.i = o.i + o.b;
        check(14, o.i, "Integer field plus Byte field");
    }

    static void arrays() {
        Integer[] a = {1, 2};
        a[0]++;
        check(2, a[0], "Integer element ++");
        int old = a[1]--;
        check(2, old, "postfix value of an Integer element");
        check(1, a[1], "Integer element after postfix");
        Character[] cs = {'a'};
        cs[0]++;
        check('b', cs[0], "Character element ++");
        Byte[] bs = {1};
        int ob = bs[0]++;
        check(1, ob, "postfix value of a Byte element");
        check((byte) 2, bs[0], "Byte element after postfix");
        Long[] ls = {5L};
        ++ls[0];
        check(6L, ls[0], "Long element prefix ++");
        int idx = 0;
        Integer[] side = {10, 20};
        side[idx++]++;
        check(11, side[0], "element with a side effect in the index");
        check(1, idx, "the index evaluated once");
    }

    static void compound() {
        WrapperIncrementTest o = new WrapperIncrementTest();
        Integer i = 5;
        i += 2;
        i *= 3;
        i <<= 1;
        i -= 2;
        check(40, i, "Integer local compound");
        Long l = 6L;
        l += 4;
        l *= 2L;
        l >>= 1;
        check(10L, l, "Long local compound");
        Float f = 1.5f;
        f += 1;
        f *= 2;
        check(5.0f, f, "Float local compound");
        Double d = 2.5;
        d += 1;
        d /= 2;
        check(1.75, d, "Double local compound");
        o.i += 5;
        o.i %= 4;
        check(2, o.i, "Integer field compound");
        o.l += 1;
        o.l <<= 2;
        check(28L, o.l, "Long field compound");
        o.f -= 0.5f;
        check(1.0f, o.f, "Float field compound");
        o.d *= 2;
        check(5.0, o.d, "Double field compound");
        Integer[] a = {4};
        a[0] += 6;
        a[0] >>= 1;
        check(5, a[0], "Integer element compound");
        Double[] ds = {1.0};
        ds[0] += 0.5;
        check(1.5, ds[0], "Double element compound");
        Integer chained = (i += 1);
        check(41, chained, "the value of a compound assignment");
    }

    public static void main(String[] args) {
        compound();
        locals();
        fields();
        arrays();
        if (failures != 0) {
            System.exit(1);
        }
        System.out.println("WrapperIncrementTest passed!");
    }
}
