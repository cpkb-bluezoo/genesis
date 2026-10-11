import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;

/* Regression test: an expression-bodied lambda's body is typed against what the
 * functional interface returns, so a generic call in it takes that type
 * ("(trimmed) -> this.gson.fromJson(trimmed, type)" for a
 * Function<String, Map<String, Object>> - Spring Boot's GsonJsonParser). */
public class LambdaExpressionBodyGenericTargetTest {
    @SuppressWarnings("unchecked")
    static <T> T from(String text, Object type) {
        Map<String, Object> m = new HashMap<>();
        m.put("text", text);
        return (T) m;
    }

    static Map<String, Object> parse(String json, Function<String, Map<String, Object>> parser) {
        return parser.apply(json.trim());
    }

    public static void main(String[] args) {
        Map<String, Object> m = parse(" abc ", (trimmed) -> from(trimmed, null));
        if (!"abc".equals(m.get("text"))) {
            throw new RuntimeException("generic call in lambda body");
        }
        System.out.println("LambdaExpressionBodyGenericTargetTest passed!");
    }
}
