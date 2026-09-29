/*
 * Regression test: a loop whose body is a try/catch/finally where BOTH
 * the try block AND a catch clause can fall through normally (in
 * addition to `break`/`continue` also being reachable from inside the
 * try, each declaring their own extra local along the way) - exactly
 * gumdrop's own ScheduledTimer.run() shape - used to throw:
 *
 *   java.lang.VerifyError: Inconsistent stackmap frames at branch target N
 *   Reason: Type top (current frame, locals[k]) is not assignable to
 *   'java/lang/Throwable' (stack map, locals[k])
 *
 * This surfaced immediately after extending an earlier fix (a `return`
 * inside try-with-finally now correctly runs the finally block first -
 * see TryFinallyEarlyReturnVerifyTest.java) to also cover `break`/
 * `continue` (needed for this exact shape, where the lock must be
 * released on every early exit from the loop body, not just early
 * returns).
 *
 * Root cause: codegen_stmt.c's AST_TRY_STMT join-point frame computation
 * (the code right after generating the try body, all catch clauses, and
 * - if a finally clause exists - its escape-handler segment) needs to
 * record a stack-map frame reflecting whatever locals are guaranteed
 * live on every real incoming edge into the code that follows the whole
 * try/catch/finally. When BOTH the try body and a catch clause can each
 * fall through normally, the existing logic tried to "merge" their
 * locals by trimming mg->stackmap's *ambient* "current" state down to
 * the smaller of the two counts - but by that point, if a finally clause
 * exists, its escape-handler segment (which restores mg->stackmap to
 * try-entry state and adds its own exception-storage local, purely for
 * its own internal bookkeeping) has ALREADY overwritten that ambient
 * state - and that segment is not a real predecessor of the join point
 * at all (it always ends in an athrow or an early return). The merge
 * logic also never restored real snapshotted TYPE information from
 * either edge, only ever trimming a *count*. Fixed by:
 * 1. Saving a proper snapshot of each catch clause's own real
 *    fallthrough-exit state (mirroring the try body's own try_exit_state,
 *    already saved correctly) - keeping the smallest one across multiple
 *    falling-through catch clauses.
 * 2. Rewriting the join-point computation to always explicitly restore
 *    from a real saved snapshot (the try body's, a catch clause's, or -
 *    when both are live - whichever has fewer locals, then trimming to
 *    the true minimum of the two) rather than ever trusting mg-
 *    >stackmap's ambient "current" state, which the finally clause's own
 *    escape-handler segment may have clobbered in the meantime.
 */
import java.util.LinkedList;
import java.util.concurrent.locks.ReentrantLock;

public class TryCatchFinallyBreakContinueVerifyTest {
    private final ReentrantLock lock = new ReentrantLock();
    private final LinkedList<Integer> queue = new LinkedList<>();
    private volatile boolean active = true;
    private int iterations = 0;

    public void run() {
        active = true;
        Integer toDispatch = null;

        while (active) {
            lock.lock();
            try {
                iterations++;
                while (active && queue.isEmpty()) {
                    if (iterations > 1) {
                        active = false;
                        break;
                    }
                    queue.add(1);
                }

                if (!active) {
                    break;
                }

                Integer next = queue.isEmpty() ? null : queue.poll();
                if (next == null) {
                    continue;
                }

                toDispatch = next;
            } catch (RuntimeException e) {
                if (!active) {
                    break;
                }
            } finally {
                lock.unlock();
            }

            if (toDispatch != null) {
                toDispatch = null;
            }
        }
    }

    public static void main(String[] args) {
        TryCatchFinallyBreakContinueVerifyTest t = new TryCatchFinallyBreakContinueVerifyTest();
        t.run();
        if (t.lock.getHoldCount() != 0) {
            throw new RuntimeException("lock leaked! holdCount=" + t.lock.getHoldCount());
        }
        System.out.println("TryCatchFinallyBreakContinueVerifyTest passed! iterations=" + t.iterations);
    }
}
