import java.util.function.Supplier;

/* Regression test: of two overloads, one taking an interface that has more than
 * one abstract method and one taking a Supplier, a lambda is for the Supplier
 * (Spring Boot's DeferredLogFactory.getLog(Log) / getLog(Supplier<Log>)). */
interface TwoMethods {
    void info(String s);

    void warn(String s);
}

public class LambdaOverloadNonFunctionalInterfaceTest {
    static String pick(TwoMethods log) {
        return "log";
    }

    static String pick(Supplier<TwoMethods> supplier) {
        return "supplier";
    }

    public static void main(String[] args) {
        TwoMethods impl = new TwoMethods() {
            public void info(String s) {
            }

            public void warn(String s) {
            }
        };
        if (!"supplier".equals(pick(() -> impl)) || !"log".equals(pick(impl))) {
            throw new RuntimeException("overload");
        }
        System.out.println("LambdaOverloadNonFunctionalInterfaceTest passed!");
    }
}
