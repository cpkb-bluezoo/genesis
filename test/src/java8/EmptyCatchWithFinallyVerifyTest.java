/*
 * Regression test: a try/catch/finally whose catch clause is EMPTY
 * (e.g. "catch (RuntimeException e) { // ignore }") - exactly gumdrop's
 * own MailboxIndexer.run() shape - used to throw at class-LOAD time
 * (before verification even runs):
 *
 *   java.lang.ClassFormatError: Illegal exception table range in class
 *   file ...
 *
 * Root cause: codegen_stmt.c's AST_TRY_STMT case, when a finally clause
 * exists, registers an *additional* exception-handler protected range
 * covering each catch clause's own body (so an exception thrown while
 * the catch block itself runs also triggers the finally) - but it did
 * this unconditionally, using the catch body's own start/end bytecode
 * offsets without checking whether the catch body actually emitted any
 * bytecode at all. An empty catch clause emits nothing, so its
 * start == end - registering that zero-length range as an exception
 * handler entry is invalid per JVMS 4.7.3 (start_pc must be strictly
 * less than end_pc), and the JVM's classloader rejects it outright.
 * There's also nothing to protect: no instructions in an empty range can
 * ever throw. Fixed by only registering that range when the catch body
 * actually emitted bytecode (catch_end > catch_start).
 */
public class EmptyCatchWithFinallyVerifyTest {
    int value;

    void doWork() {
        try {
            value = compute();
        } catch (RuntimeException e) {
            // ignore
        } finally {
            value++;
        }
    }

    int compute() {
        return 41;
    }

    public static void main(String[] args) {
        EmptyCatchWithFinallyVerifyTest t = new EmptyCatchWithFinallyVerifyTest();
        t.doWork();
        if (t.value != 42) {
            throw new RuntimeException("expected 42, got " + t.value);
        }
        System.out.println("EmptyCatchWithFinallyVerifyTest passed!");
    }
}
