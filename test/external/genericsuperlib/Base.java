package genericsuperlib;

public abstract class Base<E extends Base<E>> {
    private long interval = 5;

    public long getInterval() {
        return interval;
    }

    public int getSize() {
        return 2;
    }

    @SuppressWarnings("unchecked")
    public E self() {
        return (E) this;
    }

    public E interval(long v) {
        interval = v;
        return self();
    }
}
