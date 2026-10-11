import java.util.Optional;

/* Regression test: the type variable of a generic method that only a lambda or
 * method reference fixes ("Optional.map(Function<? super T, ? extends U>)") is
 * inferred when the call is the receiver of another call: it was left as
 * "? extends U" and "o.map(s -> s + x).orElse("")" did not type check. */
public class OptionalMapChainInferenceTest {
    static String lambda(Optional<String> o) {
        return o.map(s -> s + "x").orElse("");
    }

    static int boxed(Optional<String> o) {
        return o.map(s -> s.length()).orElse(0);
    }

    static String reference(Optional<String> o) {
        return o.map(String::trim).orElse("none");
    }

    static int twice(Optional<String> o) {
        return o.map(String::trim).map(String::length).orElse(-1);
    }

    public static void main(String[] args) {
        if (!"ax".equals(lambda(Optional.of("a"))) || boxed(Optional.of("abc")) != 3
                || !"b".equals(reference(Optional.of(" b "))) || !"none".equals(reference(Optional.empty()))
                || twice(Optional.of(" ab ")) != 2 || twice(Optional.empty()) != -1) {
            throw new RuntimeException("optional chain");
        }
        System.out.println("OptionalMapChainInferenceTest passed!");
    }
}
