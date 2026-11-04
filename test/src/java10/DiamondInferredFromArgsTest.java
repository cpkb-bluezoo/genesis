import java.util.Optional;

/*
 * A diamond with no target type takes its type arguments from the constructor
 * arguments (JLS 15.9.3, 18.5.2): "new Pair<>(parts, port)" is a Pair<String,
 * Integer>, so a later stage of the chain can pass pair.second() on to an int
 * parameter.
 */
public class DiamondInferredFromArgsTest {
    static class Pair<T, U> {
        final T first;
        final U second;

        Pair(T first, U second) {
            this.first = first;
            this.second = second;
        }

        T first() { return first; }
        U second() { return second; }
    }

    static String join(String s, int i) {
        return s + ":" + i;
    }

    static Optional<String> chain(Optional<String> text) {
        return text.flatMap(parts -> Optional.of(7).map(port -> new Pair<>(parts, port)))
                .map(pair -> join(pair.first(), pair.second()));
    }

    public static void main(String[] args) {
        var pair = new Pair<>("a", 1);
        String joined = join(pair.first(), pair.second());
        if (!joined.equals("a:1")) {
            throw new AssertionError(joined);
        }
        if (!chain(Optional.of("x")).get().equals("x:7")) {
            throw new AssertionError(chain(Optional.of("x")));
        }
        System.out.println("DiamondInferredFromArgsTest passed!");
    }
}
