import java.util.function.Consumer;

/*
 * Regression test: a lambda in a STATIC method that declares a local variable named like an
 * instance field of the class was taken for a lambda using the field, so it captured "this"
 * (the first argument of the static method, a different type): VerifyError at class load.
 */
public class StaticLambdaLocalShadowsFieldTest {

    private final String property = "field";

    static String run(String source) {
        StringBuilder out = new StringBuilder();
        Consumer<String> action = (name) -> {
            String property = source + name;
            if (property != null) {
                out.append(property);
            }
        };
        action.accept("!");
        return out.toString();
    }

    public static void main(String[] args) {
        String result = run("a");
        if (!"a!".equals(result)) {
            throw new RuntimeException("FAIL: " + result);
        }
        System.out.println("StaticLambdaLocalShadowsFieldTest passed!");
    }
}
