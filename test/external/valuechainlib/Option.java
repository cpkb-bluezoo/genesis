package valuechainlib;

public final class Option<T> implements Value<T> {
    private final T value;

    private Option(T value) {
        this.value = value;
    }

    public static <T> Option<T> of(T value) {
        return new Option<T>(value);
    }

    public static <T> Option<T> none() {
        return new Option<T>(null);
    }

    public boolean isEmpty() {
        return value == null;
    }

    public T get() {
        return value;
    }
}
