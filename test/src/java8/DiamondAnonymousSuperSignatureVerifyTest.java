import java.lang.reflect.ParameterizedType;
import java.util.Map;

/**
 * Regression: "new ParameterizedTypeReference<>() { }" used to get class
 * Signature L...ParameterizedTypeReference&lt;TT;&gt;; (unbound formal T) instead
 * of the inferred type argument, causing TypeNotPresentException at reflection
 * (Spring WebMvcEndpointChildContextConfigurationIntegrationTests).
 */
public class DiamondAnonymousSuperSignatureVerifyTest {
    static class TypeRef<T> {
    }

    static void accept(TypeRef<Map<String, ?>> ref) {
        Class<?> c = ref.getClass();
        ParameterizedType pt = (ParameterizedType) c.getGenericSuperclass();
        String sig = pt.toString();
        if (!sig.contains("Map") || sig.contains("Type T")) {
            throw new RuntimeException("bad generic super: " + sig);
        }
    }

    static void acceptDiamond(TypeRef<Map<String, ?>> ref) {
        accept(ref);
    }

    public static void main(String[] args) {
        accept(new TypeRef<Map<String, ?>>() {
        });
        acceptDiamond(new TypeRef<>() {
        });
        System.out.println("PASS");
    }
}
