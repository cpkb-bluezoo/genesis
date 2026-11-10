/**
 * Module-directive words (to, with, opens, exports, ...) are contextual
 * keywords and remain valid method, field, parameter and record component
 * names outside a module declaration.
 */
public class ContextualKeywordNamesTest {
    record R(int to, String from, int with) {}

    static class Range {
        int to = 7;
        int from() { return 3; }
        int to() { return to; }
        int with(int opens) { return opens + 1; }
    }

    public static void main(String[] args) {
        R r = new R(1, "a", 2);
        Range g = new Range();
        int exports = r.to() + r.with() + g.to() + g.from() + g.with(4);
        if (!r.from().equals("a") || exports != 1 + 2 + 7 + 3 + 5) {
            throw new AssertionError("contextual names: " + exports);
        }
        System.out.println("ContextualKeywordNamesTest passed!");
    }
}
