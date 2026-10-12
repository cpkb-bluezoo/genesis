import java.util.Collections;
import java.util.Iterator;

/**
 * Regression: new Binder(new MockConfigurationPropertySource(...)) must use varargs ctor,
 * not Iterable&lt;ConfigurationPropertySource&gt;, when the source iterates names not sources.
 */
public class BinderVarargsMockPropertySourceVerifyTest {

    interface ConfigurationPropertySource {}

    static final class ConfigurationPropertyName {}

    static final class MockConfigurationPropertySource implements ConfigurationPropertySource,
            Iterable<ConfigurationPropertyName> {
        @Override
        public Iterator<ConfigurationPropertyName> iterator() {
            return Collections.emptyIterator();
        }
    }

    static final class Binder {
        Binder(ConfigurationPropertySource... sources) {
            if (sources == null || sources.length != 1 || !(sources[0] instanceof MockConfigurationPropertySource)) {
                throw new RuntimeException("expected varargs ctor");
            }
        }

        Binder(Iterable<ConfigurationPropertySource> sources) {
            throw new RuntimeException("must not pick Iterable ctor");
        }
    }

    public static void main(String[] args) {
        new Binder(new MockConfigurationPropertySource());
        System.out.println("PASS");
    }
}
