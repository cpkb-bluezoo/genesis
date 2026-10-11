import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

/* Regression test: a method inherited through a super-interface, called on the
 * result of another call (m.keySet().stream()), is typed in the receiver's own
 * type arguments, not the declaring interface's variables ("got E"). With
 * Map.Entry::getKey in the chain, "got ? extends R". */
public class StreamChainFromCallReceiverTest {
    static String first(Map<String, Integer> m) {
        return m.keySet().stream().findFirst().get();
    }

    static Optional<String> aliasFor(Map<String, Set<String>> aliases, String alias) {
        return aliases.entrySet().stream().filter((e) -> e.getValue().contains(alias)).map(Map.Entry::getKey)
                .findFirst();
    }

    static int firstValue(Map<String, Integer> m) {
        return m.entrySet().stream().map(e -> e.getValue()).findFirst().get();
    }

    public static void main(String[] args) {
        Map<String, Integer> m = new TreeMap<>();
        m.put("a", 7);
        Map<String, Set<String>> aliases = new TreeMap<>();
        aliases.put("canonical", new TreeSet<>(Set.of("x", "y")));
        if (!"a".equals(first(m)) || firstValue(m) != 7 || !"canonical".equals(aliasFor(aliases, "y").get())
                || aliasFor(aliases, "z").isPresent()) {
            throw new RuntimeException("chain");
        }
        System.out.println("StreamChainFromCallReceiverTest passed!");
    }
}
