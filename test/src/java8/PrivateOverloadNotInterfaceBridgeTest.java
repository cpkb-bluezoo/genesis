/*
 * Regression test: a private "get(String, Class<T>, boolean)" is not an implementation of the
 * inherited interface's default "get(String, Class<T>, T)", so no bridge may forward to it
 * (VerifyError for Spring's ConfigurationPropertySourcesPropertyResolver).
 */
public class PrivateOverloadNotInterfaceBridgeTest {

    interface Resolver {
        default <T> T get(String key, Class<T> type, T fallback) {
            return fallback;
        }
    }

    abstract static class AbstractResolver implements Resolver {
    }

    static class Concrete extends AbstractResolver {
        String read(String key) {
            return get(key, String.class, true);
        }

        private <T> T get(String key, Class<T> type, boolean nested) {
            return null;
        }
    }

    public static void main(String[] args) {
        Resolver resolver = new Concrete();
        String actual = resolver.get("k", String.class, "dflt");
        if (!"dflt".equals(actual) || new Concrete().read("k") != null) {
            throw new RuntimeException("FAIL: " + actual);
        }
        System.out.println("PASS");
    }
}
