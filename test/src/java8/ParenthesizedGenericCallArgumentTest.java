import java.util.Collections;
import java.util.Map;
import java.util.function.Consumer;

/*
 * Regression test: a generic call written in parentheses as an argument
 * ("take((check((m) -> m.size())))", Mockito's "setMappings((assertArg((m) -> ...)))") was typed
 * apart from the parameter it fills: its type variable stayed open and the lambda's parameter
 * was an Object (NoSuchMethodError at run time). The parentheses change nothing.
 */
public class ParenthesizedGenericCallArgumentTest {

    static int seen;

    static <T> T check(Consumer<T> consumer) {
        return null;
    }

    static void take(Map<String, Integer> map) {
    }

    static void run() {
        take((check((m) -> seen = m.size())));
        take(check((m) -> seen += m.size()));
    }

    public static void main(String[] args) {
        run();
        take(Collections.<String, Integer>emptyMap());
        System.out.println("PASS");
    }
}
