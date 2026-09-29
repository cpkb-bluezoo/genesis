/*
 * Regression test: a "for (;;)" infinite loop (empty condition) whose
 * body opens with a "synchronized (field)" block that assigns a local
 * then conditionally returns - exactly gumdrop's own Gumdrop.addClient()
 * shape - used to throw:
 *
 *   java.lang.VerifyError: Bad type on operand stack
 *   Reason: Type '...' (current frame, stack[0]) is not assignable to
 *   float
 *
 * Root cause: codegen_stmt.c's AST_FOR_STMT case has two separate guards
 * around the loop's forward exit branch - one when deciding whether to
 * emit it ("if (condition && condition->type != AST_EMPTY_STMT)",
 * leaving "branch_pos" at its initial 0 when there's no real condition),
 * and a second, later one deciding whether to *backpatch* that branch's
 * target ("if (condition)" alone, missing the same AST_EMPTY_STMT
 * check). The parser always represents an omitted for-loop condition as
 * a non-NULL AST_EMPTY_STMT placeholder, not NULL - so for "for (;;)",
 * the first guard correctly skips emitting any branch (branch_pos stays
 * 0), but the second guard still fires anyway (its non-NULL check alone
 * doesn't distinguish "no real condition" from "a real one"), patching a
 * 2-byte branch-target value into mg->code->code[branch_pos+1] and
 * [branch_pos+2] - i.e. bytes 1-2 of the METHOD ITSELF - clobbering
 * whatever real instruction happens to live there (here, the first
 * "getfield" of the synchronized block's own lock-field expression,
 * right after the method's opening "aload_0"). Fixed by making the
 * second guard match the first exactly (or deriving both from one
 * shared boolean), so the backpatch only runs when a real branch was
 * actually emitted.
 */
public class InfiniteForSyncEarlyReturnVerifyTest {
    private final Object lock = new Object();
    private volatile Thread pending;

    void m(Object client) {
        for (;;) {
            Thread p;
            synchronized (lock) {
                p = pending;
                if (p == null) {
                    return;
                }
            }
            wait2(p);
        }
    }

    void wait2(Thread t) {
        pending = null;
    }

    public static void main(String[] args) {
        new InfiniteForSyncEarlyReturnVerifyTest().m(new Object());
        System.out.println("InfiniteForSyncEarlyReturnVerifyTest passed!");
    }
}
