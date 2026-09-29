/*
 * Regression test: a `return` statement lexically inside a try block that
 * has a `finally` clause (e.g. "try { return x; } finally { cleanup(); }")
 * - exactly gumdrop's own ScheduledTimer.pendingCount() shape - used to
 * throw:
 *
 *   java.lang.VerifyError: Expecting a stack map frame
 *
 * and, worse, silently SKIP THE FINALLY BLOCK ENTIRELY on this path (a
 * genuine correctness bug, not just a verifier complaint - a lock
 * acquired before the try would never be released).
 *
 * Root cause (three independent, compounding pieces):
 *
 * 1. codegen_stmt.c had no mechanism at all for a `return` lexically
 *    inside a try-with-finally to run the enclosing finally block before
 *    actually returning - only synchronized statements had this
 *    (mg->sync_lock_stack / emit_pending_monitorexits()). `codegen_
 *    statement` for AST_RETURN_STMT emitted the return instruction
 *    directly, never reaching the try statement's own inlined "normal
 *    completion" copy of the finally block (which sits, unreached, right
 *    after the return). Fixed by adding an analogous mg->finally_stack /
 *    emit_pending_finally_blocks(), pushed/popped around the try body's
 *    own codegen, called from AST_RETURN_STMT right before emitting the
 *    return opcode.
 *
 * 2. Once the finally block correctly runs before the return, the try
 *    statement's own "normal completion" copy of the finally block
 *    (generated unconditionally after the try body, for the case where
 *    the try body falls through normally) becomes genuinely unreachable
 *    dead code whenever the try body's own last statement is that
 *    `return` - and, being the instruction immediately following an
 *    unconditional return, it's also invalid without a stack frame
 *    nothing records for it. Fixed by skipping that copy entirely when
 *    the try body already ends with return/throw on every path (the same
 *    reachability reasoning already used elsewhere in this file, e.g.
 *    AST_SYNCHRONIZED_STMT's own body_ends_with_return check).
 *
 * 3. Skipping that copy exposed a THIRD bug: the try statement's escape-
 *    handler (its own separate inlined copy of the finally block, run
 *    when an exception propagates out of the try body) decides whether
 *    to re-throw the caught exception by checking whether the finally
 *    block it just generated "ends with return/throw" via
 *    mg->last_opcode - but that flag was never reset before regenerating
 *    it, so it inherited a stale OP_IRETURN left over from the try
 *    body's own early return (now correctly reachable, since the normal-
 *    completion copy above it was skipped) - wrongly concluding "the
 *    finally block itself returns" and skipping the re-throw entirely,
 *    silently swallowing whatever exception the try block threw. Fixed
 *    by resetting mg->last_opcode = 0 before regenerating each inlined
 *    finally-block copy.
 */
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class TryFinallyEarlyReturnVerifyTest {
    private final Lock lock = new ReentrantLock();
    private int count = 5;

    int pendingCount() {
        lock.lock();
        try {
            return count;
        } finally {
            lock.unlock();
        }
    }

    int throwing() {
        lock.lock();
        try {
            if (count > 0) {
                throw new RuntimeException("boom");
            }
            return count;
        } finally {
            lock.unlock();
        }
    }

    public static void main(String[] args) {
        TryFinallyEarlyReturnVerifyTest t = new TryFinallyEarlyReturnVerifyTest();

        int c = t.pendingCount();
        if (c != 5) {
            throw new RuntimeException("expected 5, got " + c);
        }
        /* The finally block must have actually run (releasing the lock) -
         * verify by acquiring it again; this would hang forever if the
         * lock leaked. */
        t.lock.lock();
        t.lock.unlock();

        boolean caught = false;
        try {
            t.throwing();
        } catch (RuntimeException e) {
            caught = true;
            if (!"boom".equals(e.getMessage())) {
                throw new RuntimeException("wrong message: " + e.getMessage());
            }
        }
        if (!caught) {
            throw new RuntimeException("expected exception to propagate through the finally block");
        }
        /* The finally block must have released the lock on the exception
         * path too. */
        t.lock.lock();
        t.lock.unlock();

        System.out.println("TryFinallyEarlyReturnVerifyTest passed!");
    }
}
