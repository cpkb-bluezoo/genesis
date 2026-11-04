import java.util.Optional;

public interface Value<T> {
    default Optional<T> toJavaOptional() {
        return Optional.empty();
    }
}
