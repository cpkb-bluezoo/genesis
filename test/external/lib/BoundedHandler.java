package lib;

public interface BoundedHandler<T extends Enum<T>> {
    boolean token(T type);
}
