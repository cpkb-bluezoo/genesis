import java.util.Iterator;
import java.util.Map;
import java.util.LinkedHashMap;

/**
 * Regression: new Binder(mockSource) must resolve to the varargs constructor, not
 * Iterable, when the mock is a ConfigurationPropertySource that also iterates names.
 */
public class BinderVarargsOverIterableVerifyTest {

    interface ConfigurationPropertySource {}

    static final class MockSource implements ConfigurationPropertySource, Iterable<String> {
        private final Map<String, String> map = new LinkedHashMap<>();

        MockSource(String key, String value) {
            map.put(key, value);
        }

        @Override
        public Iterator<String> iterator() {
            return map.keySet().iterator();
        }
    }

    static final class Binder {
        Binder(ConfigurationPropertySource... sources) {
            if (sources == null || sources.length != 1 || !(sources[0] instanceof MockSource)) {
                throw new RuntimeException("expected varargs ctor");
            }
        }

        Binder(Iterable<ConfigurationPropertySource> sources) {
            throw new RuntimeException("must not pick Iterable ctor");
        }
    }

    public static void main(String[] args) {
        new Binder(new MockSource("k", "v"));
        System.out.println("PASS");
    }
}
