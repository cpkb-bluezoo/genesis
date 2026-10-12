import java.util.function.Consumer;

/**
 * Regression: spec::apply as Consumer must use the classfile descriptor for
 * apply (covariant/generic return), not an erased Object return on the
 * implementation MethodHandle (WebTestClient.MockServerSpec.apply).
 */
public class MethodRefInterfaceCovariantReturnVerifyTest {

    interface Spec {
        Spec apply(Conf c);
    }

    static class Conf {
    }

    static class Impl implements Spec {
        @Override
        public Impl apply(Conf c) {
            return this;
        }
    }

    public static void main(String[] args) {
        Spec spec = new Impl();
        Consumer<Conf> consumer = spec::apply;
        consumer.accept(new Conf());
        System.out.println("PASS");
    }
}
