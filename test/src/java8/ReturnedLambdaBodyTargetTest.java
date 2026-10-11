import java.util.function.Supplier;

/* Regression test: the type a lambda is returned as is not the target of the
 * calls in its body ("return (w) -> supplier.get()" typed supplier.get() as the
 * interface being returned: Spring Boot's ApplicationContextFactory.of). */
public class ReturnedLambdaBodyTargetTest {
    interface Factory {
        Object create(String hint);

        static Factory of(Supplier<StringBuilder> supplier) {
            return (hint) -> supplier.get();
        }
    }

    public static void main(String[] args) {
        Object made = Factory.of(() -> new StringBuilder("sb")).create("ignored");
        if (!(made instanceof StringBuilder) || !"sb".equals(made.toString())) {
            throw new RuntimeException("factory");
        }
        System.out.println("ReturnedLambdaBodyTargetTest passed!");
    }
}
