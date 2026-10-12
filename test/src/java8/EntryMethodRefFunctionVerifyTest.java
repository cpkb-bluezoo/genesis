import java.util.Map;
import java.util.function.Function;

/**
 * Map.Entry::getKey for Function&lt;Entry, String&gt; (erased getKey returns Object).
 */
public class EntryMethodRefFunctionVerifyTest {

    static int which;

    static void takeStringFn(Function<Map.Entry<String, String>, String> fn) {
        which = 1;
    }

    static void takeObjectFn(Function<Map.Entry<String, String>, Object> fn) {
        which = 2;
    }

    public static void main(String[] args) {
        which = 0;
        takeStringFn(Map.Entry::getKey);
        if (which != 1) {
            throw new RuntimeException("expected String Function overload, got which=" + which);
        }
        System.out.println("PASS");
    }
}
