/* Regression test: "flag |= obj.field != null;" - a compound assignment on a
 * boolean field whose right-hand side is itself a comparison (which codegen
 * lowers to a jump pattern with its own stack-map frames). The frame at the
 * comparison's branch target disagreed with the stack actually live there
 * ("Inconsistent stackmap frames ... stack size doesn't match"). Mirrors
 * gumdrop's ArcValidator.ChainBuilder.arcSeal(). */
public class CompoundBooleanFieldComparisonVerifyTest {
    static final class Set {
        String as;
    }

    private boolean duplicate;
    private boolean other = true;

    void seal(Set set, String line) {
        duplicate |= set.as != null;
        set.as = line;
    }

    void both(Set a, Set b) {
        other &= a.as != null;
        other &= b.as == null;
    }

    public static void main(String[] args) {
        CompoundBooleanFieldComparisonVerifyTest t = new CompoundBooleanFieldComparisonVerifyTest();
        Set s = new Set();
        t.seal(s, "one");
        if (t.duplicate) {
            throw new RuntimeException("first seal flagged duplicate");
        }
        t.seal(s, "two");
        if (!t.duplicate) {
            throw new RuntimeException("second seal not flagged");
        }
        t.both(s, new Set());
        if (!t.other) {
            throw new RuntimeException("&= with comparisons wrong");
        }
        System.out.println("CompoundBooleanFieldComparisonVerifyTest passed!");
    }
}
