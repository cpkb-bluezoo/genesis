/**
 * Bug: an OUTER try-catch wrapping an INNER try-finally (no catch clause
 * of its own) whose try body always returns compiled a dead, frame-less
 * "normal completion" `goto` right after the inner try-finally's own
 * final `athrow` - "VerifyError: Expecting a stack map frame". Matches
 * gumdrop's own HttpProtocolHandler.encodeHeaders(): an outer
 * try-catch(Exception) wrapping an inner
 * "try { ...; return encoded; } finally { ByteBufferPool.release(buffer); }".
 * Root cause: AST_TRY_STMT's own "does this whole try statement
 * terminate" check (codegen_stmt.c, used to set mg->last_opcode so an
 * ENCLOSING construct's identical check sees the truth) required
 * has_catch_clauses to be true - completely missing the plain
 * try-finally (no catch at all) case, where all_catches_return simply
 * stays at its vacuous default (true) and try_ends_with_return alone
 * already correctly captures whether the whole construct terminates
 * (the finally-exception-handler path always itself terminates,
 * regardless of catches, via either the finally block's own
 * return/throw or the explicit re-throw right after it).
 */
public class NestedTryFinallyNoCatchTerminationVerifyTest {
    static int released = 0;

    static byte[] encode(boolean fail) {
        try {
            byte[] buf = new byte[4];
            try {
                if (fail) {
                    throw new RuntimeException("boom");
                }
                return buf;
            } finally {
                released++;
            }
        } catch (Exception e) {
            return new byte[0];
        }
    }

    public static void main(String[] args) {
        byte[] r1 = encode(false);
        if (r1.length != 4 || released != 1) {
            throw new RuntimeException("expected length 4, released 1, got length "
                    + r1.length + " released " + released);
        }
        byte[] r2 = encode(true);
        if (r2.length != 0 || released != 2) {
            throw new RuntimeException("expected length 0, released 2, got length "
                    + r2.length + " released " + released);
        }
    }
}
