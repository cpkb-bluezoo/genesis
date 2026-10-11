import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/*
 * Regression test: a lone generic call at a varargs position
 * ("String.format(f, parts.stream().map(..).collect(Collectors.joining(..)))") was typed by the
 * parameter "Object[]" (R of collect := Object[]), so the String it returns was cast to an array
 * (ClassCastException). Its own arguments fix what it returns.
 */
public class VarargsLoneGenericCallTest {

    interface Part {
        String blank();
    }

    static String joined(List<Part> parts) {
        return String.format("[%s] ", parts.stream().map(Part::blank).collect(Collectors.joining(" ")));
    }

    static String names(String... names) {
        return Arrays.asList(names).toString();
    }

    static String passthrough(List<String> values) {
        return names(values.stream().collect(Collectors.joining("+")));
    }

    public static void main(String[] args) {
        String actual = joined(Arrays.<Part>asList(() -> "a", () -> "b")) + passthrough(Arrays.asList("x", "y"));
        if (!"[a b] [x+y]".equals(actual)) {
            throw new RuntimeException("FAIL: " + actual);
        }
        System.out.println("PASS");
    }
}
