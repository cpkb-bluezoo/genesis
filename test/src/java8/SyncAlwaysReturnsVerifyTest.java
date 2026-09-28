/*
 * Regression test: a synchronized block whose body returns on every path
 * (no live fall-through) used to produce a class file that failed JVM
 * bytecode verification with:
 *
 *   java.lang.VerifyError: StackMapTable error: bad offset
 *   Reason: Invalid stackmap specification.
 *
 * codegen_stmt.c's AST_SYNCHRONIZED_STMT handling unconditionally emitted
 * a "normal exit" epilogue after the body (monitorexit; goto past the
 * exception handler; a stack-map frame at that goto's target) regardless
 * of whether the body could actually fall out normally. When the body
 * always returns/throws - as it does here - that epilogue, and the
 * exception handler's own unconditional rethrow, are both dead code: no
 * path reaches the goto's target at all. Recording a stack-map frame there
 * anyway pointed at an address with no real instruction when the
 * synchronized statement was the last thing in the method, since nothing
 * legitimately follows it.
 *
 * Separately (and only exposed once the above stopped failing verification
 * outright, letting the class actually load and run): a `return` lexically
 * inside a synchronized block never released the monitor at all before
 * returning, producing a runtime java.lang.IllegalMonitorStateException
 * the next time anything tried to acquire the same lock. codegen_stmt.c's
 * AST_RETURN_STMT had no awareness of enclosing synchronized statements.
 * A same-thread re-acquire can't detect this (Java monitors are reentrant
 * regardless), so this test uses a second thread to prove the lock was
 * actually released.
 * See genesis history for details (search "sync_lock_stack" and
 * "emit_pending_monitorexits" in codegen_stmt.c / codegen.h).
 */
public class SyncAlwaysReturnsVerifyTest {
    private final Object lock = new Object();

    String pick(boolean flag) {
        synchronized (lock) {
            if (flag) {
                return "yes";
            }
            return "no";
        }
    }

    public static void main(String[] args) throws Exception {
        SyncAlwaysReturnsVerifyTest t = new SyncAlwaysReturnsVerifyTest();
        if (!"yes".equals(t.pick(true))) {
            throw new RuntimeException("true branch failed");
        }
        if (!"no".equals(t.pick(false))) {
            throw new RuntimeException("false branch failed");
        }

        Acquirer acquirer = new Acquirer(t.lock);
        Thread other = new Thread(acquirer);
        other.start();
        other.join(5000);
        if (!acquirer.acquired) {
            throw new RuntimeException(
                "lock still held after returning from inside synchronized block");
        }

        System.out.println("All tests passed!");
    }

    static class Acquirer implements Runnable {
        private final Object lock;
        volatile boolean acquired;

        Acquirer(Object lock) {
            this.lock = lock;
        }

        public void run() {
            synchronized (lock) {
                acquired = true;
            }
        }
    }
}
