import java.util.function.Supplier;

/**
 * Regression: Supplier lambda metafactory must not specialize get() to return Supplier
 * (FlywayAutoConfiguration.Extension / SingletonSupplier.of(() -> ...)).
 */
public class LambdaSupplierSamReturnVerifyTest {

    interface Ext {
    }

    static final class E implements Ext {
    }

    static Object load() {
        Supplier<Ext> supplier = () -> new E();
        return supplier.get();
    }

    public static void main(String[] args) {
        if (!(load() instanceof Ext)) {
            throw new RuntimeException("expected Ext instance");
        }
        System.out.println("PASS");
    }
}
