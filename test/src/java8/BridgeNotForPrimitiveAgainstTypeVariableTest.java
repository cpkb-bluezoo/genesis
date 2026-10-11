/*
 * Regression test: an override "get(String, Class<T>, boolean)" was matched
 * against the superclass's unrelated overload "get(String, Class<T>, T)"
 * (a type variable position accepted any parameter), so a bridge with the
 * erased signature of the latter was generated, forwarding its Object
 * argument to a boolean parameter: VerifyError when the class was loaded.
 */
class Source {
    <T> T get(String key, Class<T> type, T defaultValue) {
        return defaultValue;
    }

    <T> T get(String key, Class<T> type, boolean flag) {
        return null;
    }
}

public class BridgeNotForPrimitiveAgainstTypeVariableTest extends Source {
    @Override
    <T> T get(String key, Class<T> type, boolean flag) {
        return type.cast(flag ? "yes" : "no");
    }

    public static void main(String[] args) {
        BridgeNotForPrimitiveAgainstTypeVariableTest test = new BridgeNotForPrimitiveAgainstTypeVariableTest();
        if (!"yes".equals(test.get("k", String.class, true)) || !"d".equals(test.get("k", String.class, "d"))) {
            throw new RuntimeException("FAIL: overloads got mixed up");
        }
        System.out.println("BridgeNotForPrimitiveAgainstTypeVariableTest passed!");
    }
}
