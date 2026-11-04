import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Collectors;

/*
 * "Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue)" collected from a
 * stream of entries is a Map of the entries' own key and value types: the
 * receiver of each unbound reference is the Entry the stream hands it.
 */
public class ToMapEntryMethodRefsTest {
    static String join(String key, List<String> values) {
        return key + "=" + String.join("+", values);
    }

    public static void main(String[] args) {
        var byLength = List.of("aa", "b", "cc", "").stream()
                .collect(Collectors.groupingBy(s -> s.isEmpty() ? "" : "len" + s.length()));
        var kept = byLength.entrySet().stream()
                .filter(e -> !e.getKey().isEmpty())
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
        StringBuilder out = new StringBuilder();
        for (var entry : new TreeMap<>(kept).entrySet()) {
            var values = entry.getValue();
            out.append(join(entry.getKey(), values)).append(';');
        }
        if (!out.toString().equals("len1=b;len2=aa+cc;")) {
            throw new AssertionError(out);
        }
        System.out.println("ToMapEntryMethodRefsTest passed!");
    }
}
