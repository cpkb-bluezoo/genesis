import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.Set;

/**
 * Regression: with both foo(T...) and foo(Iterable&lt;? extends T&gt;), foo(Arrays.asList(a))
 * must resolve to Iterable (RestTemplateBuilder.copiedSetOf), not varargs.
 */
public class VarargsVsIterableOverloadVerifyTest {

    static final class Helper {
        Set<String> viaVarargs(String... items) {
            return viaIterable(Arrays.asList(items));
        }

        Set<String> viaIterable(Iterable<? extends String> collection) {
            LinkedHashSet<String> set = new LinkedHashSet<>();
            collection.forEach(set::add);
            return set;
        }
    }

    public static void main(String[] args) {
        Helper h = new Helper();
        Set<String> s = h.viaVarargs("a", "b");
        if (s.size() != 2) {
            throw new RuntimeException("expected 2 elements");
        }
        System.out.println("PASS");
    }
}
