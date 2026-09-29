/*
 * Regression test: a "for (;;)" infinite loop with NO "break" anywhere
 * (every exit is either a "continue" back to the loop's own start, or the
 * loop body itself terminates via "return") - exactly gumdrop's own
 * Gumdrop.startForClient() shape - used to throw:
 *
 *   java.lang.VerifyError: StackMapTable error: bad offset
 *   Reason: Invalid stackmap specification.
 *
 * Root cause: codegen_stmt.c's AST_FOR_STMT case unconditionally recorded
 * a stack-map frame at "loop_end" (the position right after loop
 * codegen finishes) as the break target - but loop_end is only a REAL
 * jump target when the loop actually contains a "break" (patched later
 * via mg_pop_loop's break_offsets list). For a loop with no break at
 * all, loop_end is just wherever code generation happened to stop - if
 * the loop is the method's last statement, that's exactly
 * mg->code->length, one byte PAST the last real instruction. Recording
 * a frame there produces a StackMapTable entry for a nonexistent
 * instruction, which the verifier rejects. The same unconditional
 * pattern existed in AST_DO_STMT's own loop-end frame recording (for a
 * do-while whose body always terminates, skipping the condition check
 * entirely, with no break either). Fixed by only recording the loop-end
 * frame when it's genuinely reachable - gated on the loop's own
 * break_offsets list being non-empty (AST_FOR_STMT), or on the same plus
 * whether the condition check was actually emitted (AST_DO_STMT).
 *
 * Needed multiple exits (continue/synchronized/return, mirroring
 * gumdrop's real code) to naturally produce "no break anywhere" in a
 * nontrivial loop - a trivial "for(;;) { return; }" also exercises the
 * same bug, but this shape is what actually surfaced it via a real
 * multi-module smoke build.
 */
public class InfiniteForNoBreakVerifyTest {
    private final Object lock = new Object();
    private volatile Thread pending;
    private boolean started;
    private int activeCount;

    static class Loop {
        boolean isRunning() { return true; }
    }

    Loop nextLoop() {
        return new Loop();
    }

    void doStart() {
        started = true;
    }

    void awaitPendingShutdown(Thread t) {
        pending = null;
    }

    Loop start(Object client) {
        for (;;) {
            Thread p;
            boolean needsInit;
            synchronized (lock) {
                p = pending;
                if (p == null) {
                    needsInit = !started;
                    started = true;
                    activeCount++;
                } else {
                    needsInit = false;
                }
            }
            if (p != null) {
                awaitPendingShutdown(p);
                continue;
            }
            if (needsInit) {
                doStart();
            }
            Loop loop = nextLoop();
            synchronized (lock) {
                if (pending != null) {
                    continue;
                }
            }
            if (!loop.isRunning()) {
                continue;
            }
            return loop;
        }
    }

    public static void main(String[] args) {
        InfiniteForNoBreakVerifyTest t = new InfiniteForNoBreakVerifyTest();
        Loop l = t.start(new Object());
        if (l == null) {
            throw new RuntimeException("expected non-null loop");
        }
        System.out.println("InfiniteForNoBreakVerifyTest passed!");
    }
}
