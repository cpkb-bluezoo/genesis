import java.io.Closeable;
import java.io.IOException;

/*
 * "try { return value; } finally { ...; try { resource.close(); } catch
 * (IOException e) { ...; } }" - the return value (here a `long`) is
 * computed and left on the operand stack before the finally block's own
 * code is inlined and run. AST_RETURN_STMT's codegen (codegen_stmt.c)
 * left the value sitting there across emit_pending_finally_blocks(),
 * which regenerates the finally block's own statements in place - INCLUDING
 * any try/catch the finally itself contains. The JVM discards the entire
 * operand stack (keeping only the caught exception) on handler entry, so
 * the catch clause's own exit no longer has the pending return value
 * genesis's own stackmap tracking still expected there for the frame
 * shared with the finally's normal-flow edge: VerifyError "Current
 * frame's stack size doesn't match stackmap."
 *
 * Fix: store the return value to a synthetic temp local before running
 * the finally block, then reload it right before the actual return -
 * exactly what real javac always does for this reason, immune to
 * whatever branches the finally block's own code contains.
 *
 * Confirmed against gumdrop's own MaildirMailbox.endAppendMessage()
 * (org.bluezoo.gumdrop.mailbox.maildir), whose finally block's own
 * "appendChannel.close()" is wrapped in a nested try/catch.
 */
public class ReturnFinallyNestedTryVerifyTest {
    static Closeable resource;

    static long run(long value) {
        try {
            return value;
        } finally {
            if (resource != null) {
                try {
                    resource.close();
                } catch (IOException e) {
                    System.err.println("close failed: " + e);
                }
                resource = null;
            }
        }
    }

    public static void main(String[] args) throws IOException {
        resource = new Closeable() {
            public void close() throws IOException {
                throw new IOException("boom");
            }
        };
        long r = run(42L);
        if (r != 42L) {
            throw new RuntimeException("expected 42, got " + r);
        }
        System.out.println("ReturnFinallyNestedTryVerifyTest passed!");
    }
}
