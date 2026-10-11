package gchain;

public final class Content<T> {

    private final T object;

    public Content(T object) {
        this.object = object;
    }

    public T getObject() {
        return this.object;
    }
}
