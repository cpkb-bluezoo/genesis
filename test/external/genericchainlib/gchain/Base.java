package gchain;

public abstract class Base<T> {

    private final T value;

    protected Base(T value) {
        this.value = value;
    }

    public Content<T> parse(String json) {
        return new Content<T>(this.value);
    }

    public Content<T> parse(byte[] json) {
        return new Content<T>(this.value);
    }
}
