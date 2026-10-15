public class SwitchArmPatternScopeTest {
    sealed interface Action permits None, ReplaceHtml, InsertBanner {}
    record None() implements Action {}
    record ReplaceHtml(String html) implements Action {}
    record InsertBanner(String banner) implements Action {}

    // a record-pattern binding is scoped to its own arm, so a later arm may declare a local of the same name
    static String rewrite(Action action, String originalHtml) {
        return switch (action) {
            case None() -> null;
            case ReplaceHtml(String html) -> html;
            case InsertBanner(String banner) -> {
                String html = originalHtml;
                yield html + banner;
            }
        };
    }

    public static void main(String[] args) {
        if (rewrite(new None(), "o") != null) throw new AssertionError("none");
        if (!"x".equals(rewrite(new ReplaceHtml("x"), "o"))) throw new AssertionError("replace");
        if (!"ob".equals(rewrite(new InsertBanner("b"), "o"))) throw new AssertionError("insert");
        System.out.println("ok");
    }
}
