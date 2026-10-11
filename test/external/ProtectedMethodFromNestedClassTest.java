import protlib.Base;

/*
 * Regression test: a class nested in a subclass of Base (another package) called Base's
 * protected methods directly, which the JVM rejects (IllegalAccessError: JVMS 5.4.4, only a
 * subclass may): through a call without a receiver, a call on the enclosing instance, a static
 * call, and from an anonymous class. javac routes them through accessors of the enclosing class.
 */
public class ProtectedMethodFromNestedClassTest extends Base {

    class Inner {
        String implicit() {
            return describe(1, 2L, 3.5) + stat("x");
        }

        void failing() {
            fail("bad %s", "thing");
        }
    }

    static class Nested {
        String explicit(ProtectedMethodFromNestedClassTest outer) {
            return outer.describe(4, 5L, 6.5);
        }
    }

    String anonymous() {
        return new Object() {
            @Override
            public String toString() {
                return describe(7, 8L, 9.5);
            }
        }.toString();
    }

    public static void main(String[] args) {
        ProtectedMethodFromNestedClassTest outer = new ProtectedMethodFromNestedClassTest();
        String actual = outer.new Inner().implicit() + " " + new Nested().explicit(outer) + " " + outer.anonymous();
        if (!"d123.5sx d456.5 d789.5".equals(actual)) {
            throw new RuntimeException("FAIL: " + actual);
        }
        try {
            outer.new Inner().failing();
            throw new RuntimeException("FAIL: no exception");
        } catch (IllegalStateException expected) {
            if (!"bad thing".equals(expected.getMessage())) {
                throw new RuntimeException("FAIL: " + expected.getMessage());
            }
        }
        System.out.println("PASS");
    }
}
