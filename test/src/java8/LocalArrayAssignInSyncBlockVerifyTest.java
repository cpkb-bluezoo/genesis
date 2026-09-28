/*
 * Regression test: a local array variable declared before a `synchronized`
 * block, assigned to inside it, and used after it (e.g. "byte[] chunk;
 * synchronized (lock) { ...; chunk = new byte[n]; ... } ...use chunk...")
 * used to produce a classfile that failed JVM bytecode verification:
 *
 *   java.lang.VerifyError: Bad local variable type
 *   Reason: Type top (current frame, locals[k]) is not assignable to
 *   reference type
 *
 * codegen_expr.c's simple-assignment codegen for a local-variable target
 * updates mg->stackmap's tracked type for that local's slot after the
 * store - correctly for TYPE_LONG/DOUBLE/FLOAT and for TYPE_CLASS (using
 * local_info->class_name), but TYPE_ARRAY shared the TYPE_CLASS branch and
 * relied on the same local_info->class_name field, which - per its own
 * struct comment ("Class internal name (for class types, NULL otherwise)")
 * - is *never* populated for an array-typed local outside of the
 * varargs-parameter fast path. So this branch was silently a no-op for
 * "chunk = new byte[n];", leaving the slot's tracked type as whatever it
 * was before the assignment (here, still unassigned - "chunk" was declared
 * without an initializer). The synchronized statement's own join-point
 * handling (fixed earlier for a different bug this session) faithfully
 * carried that stale, too-narrow tracked state to the frame recorded after
 * the block, and any later use of "chunk" - reading it as a reference
 * type - failed verification against a frame that still claimed the slot
 * was unassigned. Fixed by building a proper array descriptor from the
 * right-hand side expression's own semantic type (value->sem_type) instead
 * - AST_NEW_ARRAY's get_expression_type() case already self-annotates this
 * (fixed for an earlier, different bug this session), and something else
 * already reliably computes it for a bare assignment-as-statement, so it
 * was simply never being consulted here.
 */
public class LocalArrayAssignInSyncBlockVerifyTest {
    final Object lock = new Object();
    int size = 10;

    long compute(long pos) {
        byte[] chunk;
        synchronized (lock) {
            if (pos >= size) {
                return 0;
            }
            int n = (int) Math.min(5L, size - pos);
            chunk = new byte[n];
            for (int i = 0; i < n; i++) {
                chunk[i] = (byte) i;
            }
        }
        long sum = 0;
        for (byte b : chunk) {
            sum += b;
        }
        return sum;
    }

    public static void main(String[] args) {
        LocalArrayAssignInSyncBlockVerifyTest t = new LocalArrayAssignInSyncBlockVerifyTest();
        long result = t.compute(0);
        if (result != 0 + 1 + 2 + 3 + 4) {
            throw new RuntimeException("expected 10, got " + result);
        }
        long early = t.compute(20);
        if (early != 0) {
            throw new RuntimeException("expected 0 for early return, got " + early);
        }
        System.out.println("All tests passed!");
    }
}
