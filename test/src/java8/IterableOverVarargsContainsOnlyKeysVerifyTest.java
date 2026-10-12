import java.util.Arrays;

/**
 * Regression: containsOnlyKeys(Iterable&lt;String&gt; from Arrays.asList) must not pick String...
 */
public class IterableOverVarargsContainsOnlyKeysVerifyTest {

    static final class AssertHelper {
        String viaVarargs(String... keys) {
            return "varargs";
        }

        String viaIterable(Iterable<? extends String> keys) {
            return "iterable";
        }

        String containsOnlyKeys(Iterable<? extends String> keys) {
            return viaIterable(keys);
        }

        String containsOnlyKeys(String... keys) {
            return viaVarargs(keys);
        }
    }

    public static void main(String[] args) {
        Iterable<String> allKeys = Arrays.asList("a", "b");
        AssertHelper h = new AssertHelper();
        if (!"iterable".equals(h.containsOnlyKeys(allKeys))) {
            throw new RuntimeException("expected Iterable overload");
        }
        System.out.println("PASS");
    }
}
