/*
 * A field of the superclass named through 'super' ("super.session", JLS 15.11.2): read,
 * assigned, updated, used in an expression and as the receiver of a call. The field may be
 * declared further up than the direct superclass, and a field of the subclass that hides it
 * does not change what 'super' names.
 */
public class SuperFieldAccessTest {
    static class Base {
        protected String session = "base";
        protected int n = 1;
        protected long total = 10L;
        protected static int counter = 5;
    }

    static class Mid extends Base {
    }

    static class Child extends Mid {
        protected String session = "child";

        String read() {
            return super.session + "/" + session;
        }

        int arithmetic() {
            return super.n * 2 + super.n;
        }

        void write() {
            super.session = "w";
            super.n = 7;
            super.n++;
            super.n += 2;
            super.total *= 3;
            --super.n;
        }

        int length() {
            return super.session.length() + super.counter;
        }

        Runnable lambda() {
            return () -> super.n += 100;
        }
    }

    public static void main(String[] args) {
        Child c = new Child();
        if (!c.read().equals("base/child") || c.arithmetic() != 3 || c.length() != 9) {
            throw new AssertionError(c.read() + " " + c.arithmetic() + " " + c.length());
        }
        c.write();
        Base b = c;
        if (!b.session.equals("w") || b.n != 9 || b.total != 30L || !c.session.equals("child")) {
            throw new AssertionError(b.session + " " + b.n + " " + b.total);
        }
        c.lambda().run();
        if (b.n != 109) {
            throw new AssertionError(b.n);
        }
        System.out.println("SuperFieldAccessTest passed!");
    }
}
