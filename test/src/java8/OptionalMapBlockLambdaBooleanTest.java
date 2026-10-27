import java.util.Optional;

public class OptionalMapBlockLambdaBooleanTest {
    private static final boolean DISABLED_BY_DEFAULT = false;

    static Optional<String> getString(String key) {
        return Optional.of("capability.check");
    }

    // A block lambda returning boolean on every path (including through try/catch)
    // infers map's U as Boolean, so orElse yields a boolean.
    private static boolean hasCapability(String customerCode) {
        return getString("cap")
            .map(capString -> {
                try {
                    return capString.length() > 0;
                } catch (Exception e) {
                    return DISABLED_BY_DEFAULT;
                }
            })
            .orElse(DISABLED_BY_DEFAULT);
    }

    public static void main(String[] args) {
        if (!hasCapability("x")) throw new AssertionError();
        System.out.println("ok");
    }
}
