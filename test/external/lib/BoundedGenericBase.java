package lib;

public class BoundedGenericBase<T extends Enum<T>> {
    protected T value;

    protected BoundedGenericBase(T value) {
        this.value = value;
    }
}
