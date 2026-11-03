package valuechainlib;

/* Does not declare toJavaOptional: it inherits Value<R>'s. */
public final class Either<L, R> implements Value<R> {
    private final R right;

    private Either(R right) {
        this.right = right;
    }

    public static <L, R> Either<L, R> right(R right) {
        return new Either<L, R>(right);
    }

    public boolean isEmpty() {
        return right == null;
    }

    public R get() {
        return right;
    }
}
