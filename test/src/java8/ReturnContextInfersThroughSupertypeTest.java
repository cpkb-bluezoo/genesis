import java.util.function.Consumer;

/*
 * Regression test: "return Throwing.of((s) -> ...)" for a method returning Consumer<String>,
 * with "static <T> Throwing<T> of(Throwing<T>)" and "Throwing<T> extends Consumer<T>", did not
 * fix T from the return type (Consumer<String> against Throwing<T> seen as Consumer<T>): the
 * lambda parameter was an Object and the call on it a NoSuchMethodError.
 */
public class ReturnContextInfersThroughSupertypeTest {

    interface Throwing<T> extends Consumer<T> {

        void run(T value) throws Exception;

        @Override
        default void accept(T value) {
            try {
                run(value);
            }
            catch (Exception ex) {
                throw new RuntimeException(ex);
            }
        }

        static <T> Throwing<T> of(Throwing<T> consumer) {
            return consumer;
        }
    }

    static StringBuilder log = new StringBuilder();

    static Consumer<String> make(String suffix) {
        return Throwing.of((s) -> log.append(s.length()).append(suffix));
    }

    public static void main(String[] args) {
        make("!").accept("abc");
        if (!"3!".equals(log.toString())) {
            throw new RuntimeException("FAIL: " + log);
        }
        System.out.println("PASS");
    }
}
