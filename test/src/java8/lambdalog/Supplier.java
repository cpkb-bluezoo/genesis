package lambdalog;

public interface Supplier<T> extends java.util.function.Supplier<T> {
    T get();
}
