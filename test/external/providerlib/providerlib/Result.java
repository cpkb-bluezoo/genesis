package providerlib;

public class Result<C> {
    private final C value;

    public Result(C value) {
        this.value = value;
    }

    public C value() {
        return this.value;
    }

    public String describe() {
        return "result of " + this.value;
    }
}
