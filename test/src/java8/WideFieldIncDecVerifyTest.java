/**
 * Bug: `obj.field++`/`obj.field--` (and prefix `++obj.field`) on a WIDE
 * instance field (long/double) always used the narrow-type bytecode
 * pattern (DUP_X1/ICONST_1/IADD), regardless of the field's real type -
 * codegen_expr.c's AST_FIELD_ACCESS increment/decrement path never
 * checked the field's actual kind, unlike the sibling AST_ARRAY_ACCESS
 * path just below it (already wide-aware) and the sibling local-variable
 * path above it (also already wide-aware). A long/double field's value
 * was silently corrupted (added as if it were an int), and DUP_X1 on a
 * wide value is rejected outright by the verifier ("Type long_2nd ...
 * not assignable to category1 type") - confirmed against gumdrop's own
 * SecondaryZoneRefresher.check(), whose "state.generation++" on a long
 * field hits exactly this path.
 */
public class WideFieldIncDecVerifyTest {
    static class Box {
        long generation;
        double weight;
        int narrow;
    }

    public static void main(String[] args) {
        Box box = new Box();
        box.generation = 5;
        long oldGen = box.generation++;
        if (oldGen != 5 || box.generation != 6) {
            throw new RuntimeException("long postfix ++ failed: old=" + oldGen + " new=" + box.generation);
        }

        long newGen = ++box.generation;
        if (newGen != 7 || box.generation != 7) {
            throw new RuntimeException("long prefix ++ failed: new=" + newGen + " field=" + box.generation);
        }

        long decOld = box.generation--;
        if (decOld != 7 || box.generation != 6) {
            throw new RuntimeException("long postfix -- failed: old=" + decOld + " new=" + box.generation);
        }

        box.weight = 2.5;
        double oldWeight = box.weight++;
        if (oldWeight != 2.5 || box.weight != 3.5) {
            throw new RuntimeException("double postfix ++ failed: old=" + oldWeight + " new=" + box.weight);
        }

        double newWeight = --box.weight;
        if (newWeight != 2.5 || box.weight != 2.5) {
            throw new RuntimeException("double prefix -- failed: new=" + newWeight + " field=" + box.weight);
        }

        box.narrow = 10;
        int oldNarrow = box.narrow++;
        if (oldNarrow != 10 || box.narrow != 11) {
            throw new RuntimeException("int postfix ++ failed (regression): old=" + oldNarrow + " new=" + box.narrow);
        }
    }
}
