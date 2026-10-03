package indirectbridgelib;

public interface Provider<S> {
    S open(String name);
}
