import io.micrometer.common.KeyValues;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

/**
 * KeyValues.of(iterable, Entry::getKey, Entry::getValue) as in Spring Boot micrometer.
 */
public class KeyValuesOfEntryMethodRefVerifyTest {

    public static void main(String[] args) {
        Map<String, String> map = Map.of("a", "b");
        Set<Map.Entry<String, String>> entries = map.entrySet();
        KeyValues kv = KeyValues.of(entries, Entry::getKey, Entry::getValue);
        if (kv == null) {
            throw new RuntimeException("null KeyValues");
        }
        System.out.println("PASS");
    }
}
