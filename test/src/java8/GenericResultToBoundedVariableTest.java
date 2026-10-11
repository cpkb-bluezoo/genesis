/*
 * Regression test: "T builder = make()" for "<T> T make()" in a method with
 * "<T extends Runnable>" stored the Object the call returns in a variable whose erasure is
 * Runnable, without a cast (VerifyError when a Runnable method was called on it; Spring's
 * "T builder = mock()" in a "<T extends ManagedChannelBuilder<T>>" test method). The empty array
 * a reified "T..." parameter takes is of the bound's class, not Object's.
 */
public class GenericResultToBoundedVariableTest {

    static int ran;

    static Class<?> reified;

    @SafeVarargs
    static <T> T reify(T... array) {
        reified = array.getClass().getComponentType();
        return null;
    }

    static <R extends Runnable> void reified() {
        R unused = reify();
    }

    @SuppressWarnings("unchecked")
    static <T> T make() {
        return (T) (Runnable) () -> ran++;
    }

    static <R extends Runnable> int run() {
        R runnable = make();
        runnable.run();
        return ran;
    }

    static <R extends Comparable<R>> int compare(R other) {
        R made = make2(other);
        return made.compareTo(other);
    }

    @SuppressWarnings("unchecked")
    static <T> T make2(T prototype) {
        return prototype;
    }

    public static void main(String[] args) {
        reified();
        if (reified != Runnable.class) {
            throw new RuntimeException("FAIL: " + reified);
        }
        if (run() != 1 || compare("a") != 0) {
            throw new RuntimeException("FAIL: " + ran);
        }
        System.out.println("PASS");
    }
}
