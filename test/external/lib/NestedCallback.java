package lib;

public class NestedCallback {
    public interface Gc<T> {
        void done(T result);
    }
}
