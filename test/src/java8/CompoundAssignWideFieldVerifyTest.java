/*
 * Regression test: a compound assignment (`+=`, `-=`, etc.) to a `long` or
 * `double` field of the current class, referenced without an explicit
 * receiver (e.g. `position += n;` rather than `this.position += n;`), used
 * as a bare expression statement (its result discarded) - produced a
 * classfile that failed JVM bytecode verification:
 *
 *   java.lang.VerifyError: Bad type on operand stack (or, depending on how
 *   the miscount happened to interact with max_stack computation,
 *   "Operand stack overflow")
 *   Reason: Type long_2nd (current frame, stack[k]) is not assignable to
 *   category1 type
 *
 * codegen_expr.c's compound-assignment codegen for a bare-identifier
 * target resolving to the *current class's own* instance field (as
 * opposed to the sibling branches for `this.field`, an inherited field, or
 * a static field, all three of which already did this correctly) loaded
 * the field's current value with GETFIELD but never told mg->stack_depth
 * how many slots that pushed - a stale comment claimed "getfield pops ref,
 * pushes value - net 0", true only for a category-1 (int-sized) field, not
 * for a category-2 (long/double) one, where GETFIELD nets +1 slot (a
 * 1-slot ref replaced by a 2-slot value). With the tracked depth
 * undercounting the real bytecode stack from that point on,
 * AST_EXPR_STMT's later "pop vs pop2" choice for discarding a compound
 * assignment's result (sized purely from the tracked stack_depth delta)
 * used a single POP where the real stack held a 2-slot long value, which
 * the verifier rejects outright. Fixed by popping the consumed ref and
 * pushing by the field's actual kind, mirroring the three sibling
 * branches that already did this.
 */
public class CompoundAssignWideFieldVerifyTest {
    private long position;
    private double total;

    void advance(int n) {
        position += n;
    }

    void accumulate(float amount) {
        total += amount;
    }

    public static void main(String[] args) {
        CompoundAssignWideFieldVerifyTest t = new CompoundAssignWideFieldVerifyTest();
        t.advance(5);
        t.advance(10);
        if (t.position != 15) {
            throw new RuntimeException("expected position 15, got " + t.position);
        }

        t.accumulate(1.5f);
        t.accumulate(2.5f);
        if (t.total != 4.0) {
            throw new RuntimeException("expected total 4.0, got " + t.total);
        }

        System.out.println("All tests passed!");
    }
}
