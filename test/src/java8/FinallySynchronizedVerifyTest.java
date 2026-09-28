/*
 * Regression test: a `try { ... } finally { synchronized (lock) { ... } }`
 * construct, where the try block has no catch clause (so an escaped
 * exception must run the finally block and then re-throw), used to fail in
 * three independent ways, all in the same area of codegen_stmt.c's
 * AST_TRY_STMT handling:
 *
 * 1. The finally block's AST is code-generated once per exit edge from the
 *    try (the normal-completion path, and the uncaught-exception escape
 *    handler here, plus once per catch clause when present) - each is a
 *    fully separate codegen_statement() call over the identical subtree.
 *    The nested `synchronized` statement allocates its own lock-object temp
 *    local via a plain mg->next_slot++, with no save/restore around each
 *    copy - so the two copies ended up with the temp in *different* local
 *    slots (whatever mg->next_slot happened to be at each call site). Since
 *    both copies' control flow reconverges after the whole try-statement,
 *    this produced a genuine mismatch the verifier correctly rejected:
 *
 *      java.lang.VerifyError: Inconsistent stackmap frames at branch target N
 *      Reason: Type top (current frame, locals[k]) is not assignable to
 *      'java/lang/Object' (stack map, locals[k])
 *
 *    Fixed by saving mg->next_slot before each inlined copy of the finally
 *    block and restoring it after, so every copy starts temp allocation
 *    from the same baseline and ends up with identical slot numbers.
 *
 * 2. Once (1) was fixed and this code path could be reached at all, the
 *    escape handler's "did the finally block terminate, so we can skip the
 *    re-throw" check read the last *physical byte* written to the method's
 *    bytecode buffer (mg->code->code[mg->code->length - 1]) instead of
 *    mg->last_opcode (the field statement codegen already maintains
 *    specifically to answer this "does the actually-reachable path
 *    terminate" question correctly). A synchronized statement's own
 *    exception handler ends in athrow, physically appended *after* its
 *    normal-completion monitorexit+goto - so this check misread that
 *    unrelated trailing athrow as "the finally block always throws" and
 *    skipped the re-throw entirely, silently swallowing any exception the
 *    try block threw. Fixed by using mg->last_opcode instead, mirroring the
 *    identical, already-correct idiom used earlier in this same function
 *    for the try block's own "does it always return" check.
 *
 * 3. Once the re-throw was actually emitted, it still failed verification
 *    (VerifyError: Bad local variable type - "top" not assignable to a
 *    reference type) because the caught exception's own local slot was
 *    never registered with mg->stackmap after its astore, unlike the
 *    synchronized statement's lock-object slot, which does get this
 *    treatment right after its own astore. Fixed by adding the equivalent
 *    stackmap_set_local_object() call for the exception slot.
 */
public class FinallySynchronizedVerifyTest {
    static final Object lock = new Object();
    static boolean draining;

    static void runInline(Runnable task) {
        try {
            task.run();
        } finally {
            synchronized (lock) {
                draining = false;
            }
        }
    }

    public static void main(String[] args) {
        runInline(new Runnable() {
            public void run() {
                /* normal completion: nothing to do */
            }
        });
        if (draining) {
            throw new RuntimeException("expected draining to be cleared after normal completion");
        }

        boolean caught = false;
        try {
            runInline(new Runnable() {
                public void run() {
                    throw new IllegalStateException("boom");
                }
            });
        } catch (IllegalStateException e) {
            caught = true;
            if (!"boom".equals(e.getMessage())) {
                throw new RuntimeException("wrong exception message: " + e.getMessage());
            }
        }
        if (!caught) {
            throw new RuntimeException("expected exception to propagate out of runInline");
        }
        if (draining) {
            throw new RuntimeException("expected draining to be cleared after exception too");
        }

        System.out.println("All tests passed!");
    }
}
