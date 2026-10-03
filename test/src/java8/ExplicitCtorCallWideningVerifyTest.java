/* Regression test: int arguments (a negative literal, a plain literal, an
 * int variable) to an explicit this(...) or super(...) call whose
 * parameters are long/double. The arguments were pushed as ints, failing
 * verification ("Type integer is not assignable to long_2nd"). Mirrors
 * gumdrop's ImapProtocolHandler.QuotaStateImpl:
 * this(tag, command, root, user, -1, -1). */
public class ExplicitCtorCallWideningVerifyTest {
    static class Base {
        final long a;
        final double b;

        Base(long a, double b) {
            this.a = a;
            this.b = b;
        }
    }

    static class Derived extends Base {
        final String tag;

        Derived(String tag) {
            this(tag, -1, 7);
        }

        Derived(String tag, long limit, double ratio) {
            super(limit, ratio);
            this.tag = tag;
        }
    }

    static class SuperInts extends Base {
        SuperInts(int n) {
            super(n, 3);
        }
    }

    /* Inner classes extending a sibling inner class: super(...) must forward the
     * enclosing instance too. Mirrors gumdrop's Amqp1ClientRecovery, whose
     * RecoverableSender extends RecordedLink(Attach). */
    abstract class Link {
        final long id;

        Link(long id) {
            this.id = id;
        }
    }

    final class Sender extends Link {
        Sender() {
            super(42);
        }
    }

    class Inner {
        final long x;

        Inner() {
            this(-1);
        }

        Inner(long x) {
            this.x = x;
        }
    }

    public static void main(String[] args) {
        Derived d = new Derived("t");
        if (d.a != -1L || d.b != 7.0 || !"t".equals(d.tag)) {
            throw new RuntimeException("this(...) widening: " + d.a + " " + d.b);
        }
        SuperInts s = new SuperInts(5);
        if (s.a != 5L || s.b != 3.0) {
            throw new RuntimeException("super(...) widening: " + s.a + " " + s.b);
        }
        ExplicitCtorCallWideningVerifyTest outer = new ExplicitCtorCallWideningVerifyTest();
        if (outer.new Inner().x != -1L) {
            throw new RuntimeException("inner this(-1)");
        }
        if (outer.new Sender().id != 42L) {
            throw new RuntimeException("inner super(...)");
        }
        System.out.println("ExplicitCtorCallWideningVerifyTest passed!");
    }
}
