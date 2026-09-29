/*
 * Regression test: constructing a classfile-loaded (external, e.g. JDK)
 * STATIC nested class via its qualified name (e.g. "new
 * ThreadPoolExecutor.AbortPolicy()") - exactly gumdrop's own
 * StorageExecutor constructor shape - used to throw:
 *
 *   java.lang.VerifyError: Bad type on operand stack
 *   Reason: Type '...' (current frame, stack[N]) is not assignable to
 *   'java/util/concurrent/ThreadPoolExecutor'
 *
 * Root cause: semantic.c's symbol_from_classfile() determined a loaded
 * class's "static" modifier purely from the class's own top-level
 * class_file access_flags ("if (cf->access_flags & ACC_STATIC) mods |=
 * MOD_STATIC;") - but per JVMS 4.1's Table 4.1-A, ACC_STATIC isn't even
 * a valid class-level access flag at all; a nested class's "static"-ness
 * is only ever recorded in a SELF-REFERENTIAL entry of its own
 * InnerClasses attribute (every nested class's own classfile lists
 * itself there, alongside any nested classes it itself declares, each
 * with its own access_flags reflecting how it was actually declared).
 * Without consulting that, EVERY classfile-loaded nested class - static
 * or not - was treated as non-static, so codegen's "new X()" inner-class
 * handling wrongly injected an enclosing-instance argument (and built
 * the constructor call accordingly) that the real (static, no-arg-
 * constructor) class never expects.
 *
 * This was masked until a separate, unrelated fix landed
 * (resolve_type_name_with_imports() correctly joining a qualified nested
 * name with '$' instead of '.' - see current_task.md's "Status of goal
 * #1" for that fix) - before that fix, a qualified reference like
 * "ThreadPoolExecutor.AbortPolicy" never actually resolved to the real
 * class via this code path at all, so this static-detection bug was
 * simply unreachable for it. Fixed by also checking the classfile's own
 * InnerClasses attribute for a self-referential entry and using ITS
 * access_flags for MOD_STATIC.
 */
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

public class ClassfileStaticNestedClassVerifyTest {
    private final ThreadPoolExecutor executor;

    ClassfileStaticNestedClassVerifyTest(int threads, int queueCapacity) {
        ThreadFactory factory = new ThreadFactory() {
            @Override
            public Thread newThread(Runnable r) {
                Thread t = new Thread(r, "worker");
                t.setDaemon(true);
                return t;
            }
        };
        this.executor = new ThreadPoolExecutor(
                threads, threads,
                60L, TimeUnit.SECONDS,
                new LinkedBlockingQueue<Runnable>(queueCapacity),
                factory,
                new ThreadPoolExecutor.AbortPolicy());
    }

    public static void main(String[] args) {
        ClassfileStaticNestedClassVerifyTest t = new ClassfileStaticNestedClassVerifyTest(2, 10);
        if (t.executor == null) {
            throw new RuntimeException("expected non-null executor");
        }
        t.executor.shutdown();
        System.out.println("ClassfileStaticNestedClassVerifyTest passed!");
    }
}
