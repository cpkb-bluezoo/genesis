/*
 * Regression test: a try/finally (no catch clause) with a local variable
 * declared and assigned inside the try block used to produce a class file
 * that failed JVM bytecode verification with:
 *
 *   java.lang.VerifyError: Stack map does not match the one at exception
 *   handler N - Type top (current frame, locals[k]) is not assignable to
 *   '...' (stack map, locals[k])
 *
 * The synthetic exception handler generated for the finally block recorded
 * its stack map frame using the locals state as of the *end* of the try
 * block (which includes locals declared partway through it, like "buf"
 * below), instead of the state at *try block entry* (the correct frame,
 * since an exception reaching the handler could have been thrown before
 * "buf" was ever assigned). The equivalent handler generated for an
 * explicit catch clause already restored to the try-entry state correctly;
 * only the finally-only synthetic handler was missing that restore. This
 * test doesn't assert anything explicitly - if the class file is malformed,
 * the JVM itself refuses to load/run it and this test fails by crashing.
 * See genesis history for details (search "Generate finally exception
 * handler" in codegen_stmt.c).
 */
public class TryFinallyLocalVerifyTest {

    static class Resource implements AutoCloseable {
        public void close() {
            System.out.println("closed");
        }
    }

    static void run(Resource r) {
        try {
            byte[] buf = new byte[8];
            String s = new String(buf);
            System.out.println("len=" + s.length());
        } finally {
            r.close();
        }
    }

    public static void main(String[] args) {
        run(new Resource());
        System.out.println("All tests passed!");
    }
}
