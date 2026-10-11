/*
 * Regression test: a default method of an interface that overrides a superinterface's method
 * with a narrower return type ("Iter prefix(String)" for "Source prefix(String)") got no bridge
 * in the interface, so a class inheriting the default and called through the superinterface
 * threw AbstractMethodError. javac emits the bridge as a default method of the interface.
 */
public class DefaultMethodCovariantBridgeTest {

    interface Source {
        Source prefix(String prefix);

        String name();
    }

    interface Iter extends Source {
        @Override
        default Iter prefix(String prefix) {
            return new Wrapped(this, prefix);
        }

        @Override
        default String name() {
            return "iter";
        }
    }

    static class Wrapped implements Iter {
        final String prefix;

        Wrapped(Iter inner, String prefix) {
            this.prefix = prefix;
        }

        @Override
        public String toString() {
            return "Wrapped(" + this.prefix + ")";
        }
    }

    static class Impl implements Iter {
    }

    public static void main(String[] args) {
        Source source = new Impl();
        String actual = source.prefix("x") + " " + source.name();
        if (!"Wrapped(x) iter".equals(actual)) {
            throw new RuntimeException("FAIL: " + actual);
        }
        System.out.println("PASS");
    }
}
