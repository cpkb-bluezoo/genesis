/* Regression test: an int constant that fits is narrowed and then boxed when
 * assigned to Byte, Short or Character (JLS 5.2): "Byte b = 1;" compiles.
 * genesis rejected it as "cannot convert int to java.lang.Byte". A final local
 * with a constant initializer is a constant too ("byte b = k;"). */
public class ConstantNarrowingToWrapperTest {
    static final int FOUR = 4;

    static class Inner {
        int count = 12;
    }

    static class Outer {
        Inner inner = new Inner();
    }

    static Byte sb = 2;
    Short fs = 3;
    Character fc = 65;

    static Byte ret() {
        return 7;
    }

    static Short retShort() {
        return FOUR;
    }

    private static void check(boolean ok, String what) {
        if (!ok) {
            System.out.println("FAILED: " + what);
            System.exit(1);
        }
    }

    public static void main(String[] args) {
        Byte b = 1;
        Short s = 300;
        Character c = 'a' + 1;
        Character c2 = 66;
        Byte neg = -128;
        final int k = 100;
        Byte fromFinal = k;
        ConstantNarrowingToWrapperTest t = new ConstantNarrowingToWrapperTest();
        check(b == 1 && b.getClass() == Byte.class, "Byte");
        check(s == 300 && s.getClass() == Short.class, "Short");
        check(c == 'b' && c2 == 'B', "Character");
        check(neg == -128, "negative Byte");
        check(fromFinal == 100, "Byte from a final local");
        check(sb == 2 && t.fs == 3 && t.fc == 'A', "fields");
        check(ret() == 7 && retShort() == 4, "returns");
        byte prim = k;
        char primChar = k + 1;
        check(prim == 100 && primChar == 'e', "narrowing from a final local to primitives");
        Byte[] bytes = {1, -2};
        Character[] chars = {'x', 66};
        Short[][] shorts = {{3}};
        check(bytes[1] == -2 && chars[1] == 'B' && shorts[0][0] == 3, "array initializer elements");
        /* a final local is a constant only when its initializer is one: a chain of field
         * accesses is not, and must not be resolved as a type name */
        Outer outer = new Outer();
        final int fromFields = outer.inner.count;
        check(fromFields == 12, "a final local initialised from fields");
        Object o = b;
        check(o instanceof Byte, "boxed as Byte, not Integer");
        System.out.println("ConstantNarrowingToWrapperTest passed!");
    }
}
