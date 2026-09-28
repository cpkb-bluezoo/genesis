/*
 * Regression test: an anonymous class created inside a *static initializer
 * block* (`static { ... }`, as opposed to a static field's own inline
 * initializer, e.g. "private static final Executor X = new Executor()
 * {...};", already fixed earlier this session) used to throw at class-init
 * time despite compiling cleanly:
 *
 *   java.lang.NoSuchMethodError: OuterClass$1: method 'void <init>()' not
 *   found
 *
 * Same root cause and fix shape as the static-field-initializer version:
 * pass1_collect_declarations() is an iterative (stack-based) AST walker
 * that generically descends into every node's children - including a
 * static initializer block's own body, several levels down - well before
 * pass2's own, separate handling of it runs. When that generic descent
 * reaches a nested "new SomeInterface() { ... }" anonymous class
 * expression, pass1's own anonymous-class handling creates (and
 * permanently caches) its symbol based on sem->in_static_field_init - a
 * flag pass1 only ever set for AST_FIELD_DECL, never for
 * AST_INITIALIZER_BLOCK. So an anonymous class inside a static
 * initializer block was always misclassified as a non-static inner class
 * (given a synthetic this$0 field and an enclosing-instance constructor
 * parameter), while the separate call-site codegen correctly treated it
 * as static and called a no-arg constructor - the same constructor-
 * descriptor mismatch as the field-initializer case. Fixed by having
 * pass1's AST_INITIALIZER_BLOCK case also save/set/restore
 * in_static_field_init around its own subtree's traversal, exactly
 * mirroring the AST_FIELD_DECL fix.
 *
 * This exact shape matters beyond being a language-completeness gap: a
 * java.util.concurrent.ThreadFactory built with an anonymous class inside
 * a static initializer block (a common way to give pool threads a name
 * without needing an instance) hit this - and because the failure was in
 * <clinit>, exceptions thrown from a background pool thread trying to use
 * that (uninitializable) class were easy to miss entirely, surfacing only
 * as submitted work silently never running (a test's CountDownLatch.await
 * timing out with no other visible error at all).
 */
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

public class AnonClassStaticInitBlockTest {
    private static final ThreadPoolExecutor POOL;

    static {
        POOL = new ThreadPoolExecutor(2, 2, 60, TimeUnit.SECONDS,
                new LinkedBlockingQueue<Runnable>(),
                new ThreadFactory() {
                    public Thread newThread(Runnable r) {
                        Thread t = new Thread(r, "test-worker");
                        t.setDaemon(true);
                        return t;
                    }
                });
    }

    public static void main(String[] args) throws Exception {
        final CountDownLatch latch = new CountDownLatch(1);
        final int[] result = { -1 };
        POOL.execute(new Runnable() {
            public void run() {
                result[0] = 42;
                latch.countDown();
            }
        });
        if (!latch.await(5, TimeUnit.SECONDS)) {
            throw new RuntimeException("timed out waiting for pool task");
        }
        if (result[0] != 42) {
            throw new RuntimeException("expected 42, got " + result[0]);
        }
        POOL.shutdown();
        System.out.println("All tests passed!");
    }
}
