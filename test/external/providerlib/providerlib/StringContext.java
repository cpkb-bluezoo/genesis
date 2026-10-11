package providerlib;

public interface StringContext extends Context<String> {
    static StringContext of(String value) {
        return () -> new Result<String>(value);
    }
}
