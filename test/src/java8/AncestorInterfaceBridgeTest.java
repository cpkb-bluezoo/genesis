/*
 * Regression test: "Concrete<T extends Special> extends Base<T>", with the abstract
 * "Base<T extends Factory> implements Customizer<T>", declares customize(T) (erased to Special)
 * where Customizer.customize(T) is erased to Factory: Concrete needs the bridge
 * customize(Factory) (AbstractMethodError for Spring's TomcatAccessLogCustomizer through
 * AccessLogCustomizer).
 */
public class AncestorInterfaceBridgeTest {

    interface Factory {
    }

    interface Special extends Factory {
        String name();
    }

    interface Customizer<T extends Factory> {
        String customize(T factory);
    }

    abstract static class Base<T extends Factory> implements Customizer<T> {
    }

    static class Concrete<T extends Special> extends Base<T> {
        @Override
        public String customize(T factory) {
            return factory.name();
        }
    }

    static class Deeper extends Concrete<Special> {
    }

    @SuppressWarnings("unchecked")
    public static void main(String[] args) {
        Customizer<Factory> first = (Customizer<Factory>) (Customizer<?>) new Concrete<Special>();
        Customizer<Factory> second = (Customizer<Factory>) (Customizer<?>) new Deeper();
        Special special = () -> "ok";
        String actual = first.customize(special) + second.customize(special);
        if (!"okok".equals(actual)) {
            throw new RuntimeException("FAIL: " + actual);
        }
        System.out.println("PASS");
    }
}
