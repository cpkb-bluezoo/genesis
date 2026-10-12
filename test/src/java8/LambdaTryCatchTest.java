import java.util.function.Predicate;

/**
 * A catch inside a lambda must be in the synthetic method's exception table.
 * The handler bytecode alone does not catch anything.
 */
public class LambdaTryCatchTest {
    public static void main(String[] args) {
        Predicate<String> p = s -> {
            try {
                if (s.indexOf('/') < 0) {
                    throw new IllegalArgumentException(s);
                }
                return true;
            } catch (IllegalArgumentException ex) {
                return false;
            }
        };
        if (p.test("test~plain")) {
            throw new AssertionError("invalid value was not caught");
        }
        if (!p.test("text/plain")) {
            throw new AssertionError("valid value was rejected");
        }
        System.out.println("PASS");
    }
}
