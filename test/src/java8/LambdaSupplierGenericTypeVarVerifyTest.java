import java.util.function.Supplier;

/**
 * Regression: Supplier&lt;E extends ConfigurationExtension&gt; block lambda (Flyway Extension)
 * must specialize get() to E / ConfigurationExtension, not Object or Supplier.
 */
public class LambdaSupplierGenericTypeVarVerifyTest {

    interface ConfigurationExtension {}

    static final class OracleConfigurationExtension implements ConfigurationExtension {}

    static final class Extension<E extends ConfigurationExtension> {
        Extension(E ignored) {}

        static <E extends ConfigurationExtension> Supplier<E> supplier() {
            return () -> {
                E extension = (E) new OracleConfigurationExtension();
                return extension;
            };
        }
    }

    public static void main(String[] args) {
        if (!(Extension.supplier().get() instanceof OracleConfigurationExtension)) {
            throw new RuntimeException("bad supplier return type");
        }
        System.out.println("PASS");
    }
}
