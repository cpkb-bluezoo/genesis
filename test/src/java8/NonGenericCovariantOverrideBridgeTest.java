/*
 * Regression test: a covariant return override of a NON-generic method
 * ("Object apply(Object)" overridden as "String apply(Object)") got no
 * synthetic bridge at the superclass's own descriptor, so a call through
 * the superclass ran the superclass body (or threw AbstractMethodError for
 * an abstract one). Covers named classes, anonymous classes, enum constant
 * bodies and a bridge two levels up the hierarchy.
 */
public class NonGenericCovariantOverrideBridgeTest {

    static abstract class Base {
        abstract Object apply(Object o);
    }

    static class Named extends Base {
        String apply(Object o) { return "named:" + o; }
    }

    static class Concrete {
        Object apply(Object o) { return "base:" + o; }
    }

    static class Middle extends Concrete {
        CharSequence apply(Object o) { return "middle:" + o; }
    }

    static class Leaf extends Middle {
        String apply(Object o) { return "leaf:" + o; }
    }

    enum Style {
        PLAIN {
            @Override protected Object apply(Object item) { return item; }
        },
        QUOTED {
            @Override protected String apply(Object item) { return "'" + item + "'"; }
        };
        protected abstract Object apply(Object item);
        String show(Object o) { return String.valueOf(apply(o)); }
    }

    static String check(String what, Object actual, String expected) {
        if (!expected.equals(String.valueOf(actual))) {
            return "FAIL: " + what + " expected " + expected + " got " + actual + "\n";
        }
        return "";
    }

    public static void main(String[] args) {
        String failures = "";
        Base named = new Named();
        failures += check("named", named.apply("x"), "named:x");
        Base anon = new Base() {
            String apply(Object o) { return "anon:" + o; }
        };
        failures += check("anonymous", anon.apply("x"), "anon:x");
        failures += check("enum plain", Style.PLAIN.show("x"), "x");
        failures += check("enum quoted", Style.QUOTED.show("x"), "'x'");
        Concrete c = new Leaf();
        failures += check("leaf via Concrete", c.apply("x"), "leaf:x");
        Middle m = new Leaf();
        failures += check("leaf via Middle", m.apply("x"), "leaf:x");
        if (failures.length() > 0) {
            System.out.print(failures);
            System.exit(1);
        }
        System.out.println("NonGenericCovariantOverrideBridgeTest passed!");
    }
}
