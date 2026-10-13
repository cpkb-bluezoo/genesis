import java.util.Optional;

public class ExplicitGenericMethodRefTest {
    static String load(Optional<String> cache) {
        return cache
            .map(Box::<String, String>identity)
            .orElseGet(() -> "fallback");
    }

    static final class Box {
        static <T, U> T identity(T value) {
            return value;
        }
    }

    public static void main(String[] args) {
        if (!"x".equals(load(Optional.of("x")))) throw new AssertionError("present");
        if (!"fallback".equals(load(Optional.<String>empty()))) throw new AssertionError("absent");
        System.out.println("ok");
    }
}
