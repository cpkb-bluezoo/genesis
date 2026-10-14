public class SwitchExprRecordPatternTest {
    sealed interface Action permits None, Insert, Remove {}
    record None() implements Action {}
    record Insert(String banner) implements Action {}
    record Remove() implements Action {}

    static String simple(Action action) {
        return switch (action) {
            case None() -> null;
            case Insert(String banner) -> banner;
            case Remove() -> "removed";
        };
    }

    // each arm has its own scope, so "html" can be declared in several
    static String blocks(Action action, String original, String[] classes) {
        return switch (action) {
            case None() -> null;
            case Insert(String banner) -> {
                String html = original;
                for (String c : classes) {
                    html = html.replace(c, "");
                }
                yield html + banner;
            }
            case Remove() -> {
                String html = original;
                for (String c : classes) {
                    html = html.replace(c, "");
                }
                yield html;
            }
        };
    }

    public static void main(String[] args) {
        if (simple(new None()) != null || !"b".equals(simple(new Insert("b")))
                || !"removed".equals(simple(new Remove()))) {
            throw new AssertionError("simple");
        }
        String[] cls = { "x" };
        if (blocks(new None(), "axb", cls) != null
                || !"abB".equals(blocks(new Insert("B"), "axb", cls))
                || !"ab".equals(blocks(new Remove(), "axb", cls))) {
            throw new AssertionError("blocks");
        }
        System.out.println("ok");
    }
}
