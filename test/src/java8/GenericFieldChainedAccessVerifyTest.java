/**
 * Regression: chained access on a generic superclass field (erased Object) needs
 * checkcast before a subsequent getfield (DefaultBootstrapContextTests shape).
 */
public class GenericFieldChainedAccessVerifyTest {

    abstract static class Base<L> {
        protected L listener;

        Base(L value) {
            this.listener = value;
        }
    }

    static final class Target {
        int calls;

        void mark() {
            calls++;
        }
    }

    static final class AssertHelper extends Base<Target> {
        AssertHelper(Target target) {
            super(target);
        }

        void wasCalled() {
            listener.mark();
        }
    }

    public static void main(String[] args) {
        Target target = new Target();
        new AssertHelper(target).wasCalled();
        if (target.calls != 1) {
            throw new RuntimeException("bad calls");
        }
        System.out.println("PASS");
    }
}
