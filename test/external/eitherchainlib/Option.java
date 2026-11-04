import java.util.Optional;

public interface Option<T> extends Value<T> {
    static <T> Option<T> some(T value) {
        return new Option<T>() {
            public Optional<T> toJavaOptional() {
                return Optional.of(value);
            }
        };
    }
}
