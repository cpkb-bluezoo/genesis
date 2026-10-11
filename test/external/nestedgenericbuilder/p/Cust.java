package p;

public interface Cust<T> {
    String describe(T t);

    interface Builder<T> {
        default Builder<T> nested() {
            return nested(true);
        }

        Builder<T> nested(boolean nested);

        Cust<T> build();
    }
}
