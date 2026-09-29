package lib;

public class GenericSuperCtor<T> {
    protected T first;
    protected T second;

    protected GenericSuperCtor(int n, T first, T second) {
        this.first = first;
        this.second = second;
    }
}
