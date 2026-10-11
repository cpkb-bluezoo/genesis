/*
 * Regression test: an unqualified call of an Object method ("hashCode()",
 * "getClass()", "toString()") from a nested class that extends nothing was
 * compiled as a call on the ENCLOSING instance (this$0), which a static
 * nested class doesn't have: NoSuchFieldError at run time. Every class
 * inherits Object's methods, so the call is on 'this'.
 */
public class ObjectMethodInStaticNestedClassTest {

    static class Nested {
        boolean sameClass(Object other) {
            return getClass() == other.getClass();
        }

        int hash() {
            return hashCode();
        }

        String text() {
            return toString();
        }
    }

    class Inner {
        Class<?> type() {
            return getClass();
        }
    }

    public static void main(String[] args) {
        Nested nested = new Nested();
        if (!nested.sameClass(new Nested()) || nested.hash() != nested.hashCode()
                || !nested.text().equals(nested.toString())) {
            throw new RuntimeException("FAIL: Object method on the nested instance");
        }
        Inner inner = new ObjectMethodInStaticNestedClassTest().new Inner();
        if (inner.type() != Inner.class) {
            throw new RuntimeException("FAIL: getClass() in an inner class");
        }
        System.out.println("ObjectMethodInStaticNestedClassTest passed!");
    }
}
