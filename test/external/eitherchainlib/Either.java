import java.util.Optional;

public interface Either<L, R> extends Value<R> {
    static <L, R> Either<L, R> right(R value) {
        return new Either<L, R>() {
            public Optional<R> toJavaOptional() {
                return Optional.of(value);
            }
        };
    }
}
