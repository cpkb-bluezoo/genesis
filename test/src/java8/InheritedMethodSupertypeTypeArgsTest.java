import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/* Regression test: a method inherited from a supertype speaks of THAT type's
 * variables. With "MultiValueMap<K, V> extends Map<K, List<V>>", the V of
 * Map.get / Map.getOrDefault is List<V>, not MultiValueMap's V: the call was
 * checked against Runnable instead of List<Runnable> (Spring Boot's
 * ServletContextInitializerBeans). */
interface MultiValues<K, V> extends Map<K, List<V>> {
}

class Registry extends HashMap<Class<?>, List<Runnable>> implements MultiValues<Class<?>, Runnable> {
}

public class InheritedMethodSupertypeTypeArgsTest {
    static List<Runnable> all(MultiValues<Class<?>, Runnable> m, Class<?> a, Class<?> b) {
        List<Runnable> out = new ArrayList<>();
        out.addAll(m.getOrDefault(a, Collections.emptyList()));
        out.addAll(m.getOrDefault(b, Collections.emptyList()));
        List<Runnable> direct = m.get(a);
        if (direct != null) {
            out.addAll(direct);
        }
        return out;
    }

    public static void main(String[] args) {
        Registry m = new Registry();
        Runnable r = () -> { };
        m.put(String.class, List.of(r));
        List<Runnable> got = all(m, String.class, Integer.class);
        List<Runnable> viaClass = m.getOrDefault(String.class, Collections.emptyList());
        if (got.size() != 2 || viaClass.size() != 1) {
            throw new RuntimeException(got.size() + " " + viaClass.size());
        }
        System.out.println("InheritedMethodSupertypeTypeArgsTest passed!");
    }
}
