/**
 * Bug: a `try` body ending in `break` (or `continue`) - reaching
 * outside the try via its own `goto`, never falling through - still
 * got a spurious "jump past all catch handlers" goto emitted right
 * after it: unreachable, frame-less dead code. VerifyError "Expecting
 * a stack map frame" (or "Inconsistent stackmap frames", depending on
 * what data happens to occupy that dead offset). Matches gumdrop's own
 * QuicConnection.close()'s "try { sendConnectionClose(...); break; }
 * catch (PacketProtectionException e) { ...; }" inside a for-loop.
 * Root cause: `try_ends_with_return` (codegen_stmt.c, used to decide
 * whether to emit that goto) checked for RETURN/IRETURN/.../ATHROW but
 * not OP_GOTO - unlike its sibling `try_body_ends_with_return` (used
 * just above for a different purpose, whether to skip a redundant
 * finally copy), which already included it.
 */
public class TryBreakInLoopCatchVerifyTest {
    static int lastTried = -1;

    static void mayFail(int i) throws Exception {
        lastTried = i;
        if (i < 2) {
            throw new Exception("fail at " + i);
        }
    }

    static int run() {
        int[] values = { 0, 1, 2, 3 };
        int found = -1;
        for (int i = values.length - 1; i >= 0; i--) {
            int v = values[i];
            try {
                mayFail(v);
                found = v;
                break;
            } catch (Exception e) {
                continue;
            }
        }
        return found;
    }

    public static void main(String[] args) {
        int result = run();
        if (result != 3) {
            throw new RuntimeException("expected 3, got " + result);
        }
    }
}
