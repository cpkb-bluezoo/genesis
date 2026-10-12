/**
 * Regression: chained access on a generic superclass field (erased to Object in the
 * class file) must checkcast to the use-site type before the next getfield.
 */
public class GenericActualFieldChainVerifyTest {

    abstract static class Base<T> {
        protected T actual;

        Base(T value) {
            this.actual = value;
        }
    }

    static final class Target {
        final int value;

        Target(int value) {
            this.value = value;
        }
    }

    static final class SubAssert extends Base<Target> {
        SubAssert(Target target) {
            super(target);
        }

        int readValue() {
            return actual.value;
        }
    }

    public static void main(String[] args) {
        if (new SubAssert(new Target(42)).readValue() != 42) {
            throw new RuntimeException("bad value");
        }
        System.out.println("PASS");
    }
}
