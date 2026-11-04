public interface Maker<B extends Maker<B, T>, T> {
    T build();
}
