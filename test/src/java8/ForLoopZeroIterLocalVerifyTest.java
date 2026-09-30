/*
 * A local declared (without an initializer) before a C-style for-loop and
 * assigned only inside its body was recorded as definitely-assigned in the
 * loop's own exit ("loop_end"/break-target) stackmap frame, even though the
 * loop might run zero times (its own condition can be false on the very
 * first check) - a real predecessor edge into that same frame where the
 * local is still genuinely "top" (unassigned). codegen_stmt.c's AST_FOR_STMT
 * handling recorded the loop_end frame directly from whatever ambient
 * mg->stackmap state the body/update/back-edge codegen left behind, instead
 * of restoring to a snapshot taken before the condition/body (as the
 * enhanced-for loop's own loop_end already correctly does via its
 * loop_entry_state) - producing a VerifyError as soon as ANYTHING is
 * reached via that frame afterward, whether or not the stale local is
 * itself read there, since the JVM verifier compares every local in the
 * recorded frame against the real simulated state.
 *
 * Confirmed against gumdrop's own Encoder.main() (org.bluezoo.gumdrop.
 * http.hpack): "byte b;" declared without an initializer, assigned only
 * inside a "for (int i = 0; i < encoded.length && success; i++)" loop's
 * body, whose own stackmap frame right after the loop wrongly claimed b
 * was already an int.
 */
public class ForLoopZeroIterLocalVerifyTest {
    static boolean run(int[] arr, byte[] expected) {
        byte b;
        boolean success = true;
        for (int i = 0; i < expected.length && success; i++) {
            b = (byte) arr[i];
            if (b != expected[i]) {
                success = false;
            }
        }
        if (success) {
            System.out.println("all matched");
        }
        return success;
    }

    public static void main(String[] args) {
        boolean r = run(new int[] { 1, 2, 3 }, new byte[] { 1, 2, 3 });
        if (!r) {
            throw new RuntimeException("expected true");
        }
        System.out.println("ForLoopZeroIterLocalVerifyTest passed!");
    }
}
