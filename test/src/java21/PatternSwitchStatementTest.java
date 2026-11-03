import java.util.ArrayList;
import java.util.List;

/*
 * Pattern labels in a switch statement (JLS 14.11): type patterns, record
 * patterns, and statement arms that use break, return and continue.
 */
public class PatternSwitchStatementTest {
    public sealed interface Action permits None, Replace, Append {}
    public record None() implements Action {}
    public record Replace(String text) implements Action {}
    public record Append(String text, int times) implements Action {}

    static final List<String> log = new ArrayList<>();

    static void run(Action action) {
        switch (action) {
            case PatternSwitchStatementTest.None() -> { }
            case PatternSwitchStatementTest.Replace(String text) -> {
                if (text != null) {
                    log.add("replace " + text);
                }
            }
            case Append(String text, int times) -> {
                if (times > 1) {
                    log.add("append " + text + " x" + times);
                    break;
                }
                log.add("append " + text);
            }
        }
    }

    static String describe(Object o) {
        switch (o) {
            case Integer i -> {
                if (i > 10) {
                    return "big " + i;
                }
                return "int " + i;
            }
            case String s -> {
                return "string " + s;
            }
            default -> {
                return "other";
            }
        }
    }

    static int skipStrings(Object[] items) {
        int counted = 0;
        for (Object item : items) {
            switch (item) {
                case String s -> {
                    continue;
                }
                default -> counted++;
            }
            counted += 10;
        }
        return counted;
    }

    public static void main(String[] args) {
        run(new None());
        run(new Replace("hi"));
        run(new Replace(null));
        run(new Append("a", 3));
        run(new Append("b", 1));
        if (!log.equals(List.of("replace hi", "append a x3", "append b"))) {
            throw new AssertionError(log);
        }
        if (!"big 11".equals(describe(11)) || !"int 3".equals(describe(3))
                || !"string x".equals(describe("x")) || !"other".equals(describe(2.0))) {
            throw new AssertionError("describe");
        }
        if (skipStrings(new Object[] {"a", 1, "b", 2}) != 22) {
            throw new AssertionError("skipStrings");
        }
        System.out.println("PatternSwitchStatementTest passed!");
    }
}
