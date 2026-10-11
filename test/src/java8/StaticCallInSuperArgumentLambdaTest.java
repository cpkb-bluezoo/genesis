import java.util.function.Consumer;

/*
 * Regression test: a lambda in the arguments of super(...) that called a static method of its
 * class (declared in it, or inherited) by its simple name was taken to call an instance method
 * and capture "this": the invokedynamic passed the uninitialized this (VerifyError). The
 * method is static, so the lambda captures nothing but its own arguments.
 */
public class StaticCallInSuperArgumentLambdaTest {

    static StringBuilder log = new StringBuilder();

    static class Base {
        Base(Consumer<String> consumer) {
            consumer.accept("!");
        }

        static void inherited(String prefix, String value) {
            log.append("i:").append(prefix).append(value);
        }
    }

    static class Own extends Base {
        Own(String prefix) {
            super((value) -> declared(prefix, value));
        }

        private static void declared(String prefix, String value) {
            log.append("d:").append(prefix).append(value);
        }
    }

    static class Inheriting extends Base {
        Inheriting(String prefix) {
            super((value) -> inherited(prefix, value));
        }
    }

    public static void main(String[] args) {
        new Own("a");
        new Inheriting("b");
        if (!"d:a!i:b!".equals(log.toString())) {
            throw new RuntimeException("FAIL: " + log);
        }
        System.out.println("PASS");
    }
}
