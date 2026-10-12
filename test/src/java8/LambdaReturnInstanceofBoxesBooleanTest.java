import java.util.concurrent.Callable;

/**
 * A block lambda returning an instanceof test hands back a Boolean (it was boxed as an Integer,
 * failing with a ClassCastException where the Callable's value is read).
 */
public class LambdaReturnInstanceofBoxesBooleanTest {

    static <X> X until(Callable<X> c) throws Exception {
        return c.call();
    }

    static Boolean untilB(Callable<Boolean> c) throws Exception {
        return c.call();
    }

    public static void main(String[] args) throws Exception {
        Object o = args;
        if (untilB(() -> o instanceof String[]) != Boolean.TRUE) {
            throw new AssertionError("expression lambda");
        }
        if (untilB(() -> { return o instanceof String[]; }) != Boolean.TRUE) {
            throw new AssertionError("block lambda");
        }
        if (untilB(() -> { if (args.length > 3) { return false; } return o instanceof Integer; }) != Boolean.FALSE) {
            throw new AssertionError("block lambda, two returns");
        }
        Boolean inferred = until(() -> { return o instanceof String[]; });
        if (!inferred) {
            throw new AssertionError("inferred result type");
        }
        System.out.println("PASS");
    }
}
