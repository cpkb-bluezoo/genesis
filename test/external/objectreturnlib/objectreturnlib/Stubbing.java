package objectreturnlib;

public final class Stubbing<T> {
    private T value;

    private Stubbing() {
    }

    public static <T> Stubbing<T> given(T call) {
        return new Stubbing<T>();
    }

    public Stubbing<T> willReturn(T value) {
        this.value = value;
        return this;
    }

    public T value() {
        return this.value;
    }
}
