import java.util.function.Supplier;

/**
 * Minimal Flyway Extension ctor: captured locals, generic E, nested Supplier in Assert.state.
 */
public class FlywayExtensionSupplierBlockVerifyTest {

    interface ConfigurationExtension {}

    static final class OracleConfigurationExtension implements ConfigurationExtension {}

    static final class PluginRegister {
        <E extends ConfigurationExtension> E getExact(Class<E> type) {
            return type.cast(new OracleConfigurationExtension());
        }
    }

    static final class FluentConfiguration {
        PluginRegister getPluginRegister() {
            return new PluginRegister();
        }
    }

    static final class Assert {
        static void state(boolean expr, Supplier<String> message) {
            if (!expr) {
                throw new IllegalStateException(message.get());
            }
        }
    }

    static final class SingletonSupplier {
        static <T> Supplier<T> of(Supplier<T> supplier) {
            return supplier;
        }
    }

    static class Extension<E extends ConfigurationExtension> {
        private final Supplier<E> extension;

        Extension(FluentConfiguration configuration, Class<E> type, String name) {
            this.extension = SingletonSupplier.of(() -> {
                E extension = configuration.getPluginRegister().getExact(type);
                Assert.state(extension != null, () -> "Flyway %s extension missing".formatted(name));
                return extension;
            });
        }

        E get() {
            return extension.get();
        }
    }

    public static void main(String[] args) {
        Extension<OracleConfigurationExtension> ext = new Extension<>(
                new FluentConfiguration(), OracleConfigurationExtension.class, "Oracle");
        if (!(ext.get() instanceof OracleConfigurationExtension)) {
            throw new RuntimeException("bad extension type");
        }
        System.out.println("PASS");
    }
}
