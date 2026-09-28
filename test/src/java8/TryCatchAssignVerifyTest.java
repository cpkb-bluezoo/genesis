/*
 * Regression test: a local variable assigned inside a try block and used
 * afterward, where every catch clause unconditionally throws instead of
 * falling through, used to produce a class file that failed JVM bytecode
 * verification with:
 *
 *   java.lang.VerifyError: Bad local variable type
 *   Reason: Type top (current frame, locals[N]) is not assignable to
 *   reference type
 *
 * codegen_stmt.c's AST_TRY_STMT handling records the stack-map frame at the
 * join point after the try/catch using whatever mg->stackmap happens to be
 * tracking once all catch handlers finish generating. Each catch handler's
 * *entry* frame is (correctly) recorded from a restore to the try block's
 * *entry* state, since an exception can occur at any point in the try body
 * - but when every catch clause terminates (as here), the try block's own
 * exit goto is the *only* live edge into the join point, and the state
 * left behind after generating the catch handler no longer reflects the
 * try block's actual exit state (the local assigned inside try reverted to
 * "uninitialized" from the catch handler's own entry-frame restore). The
 * recorded join-point frame ended up claiming the local was still
 * unassigned, while the real bytecode reaching that point (via the try
 * block's own goto) has it assigned to a real reference type.
 * See genesis history for details (search "try_exit_state" in
 * codegen_stmt.c).
 */
public class TryCatchAssignVerifyTest {

    static String locate(String key) throws java.io.IOException {
        if (key.equals("missing")) {
            throw new java.io.FileNotFoundException(key);
        }
        return key.toUpperCase();
    }

    static String require(String key) throws java.io.IOException {
        String node;
        try {
            node = locate(key);
        } catch (java.io.FileNotFoundException e) {
            throw new java.io.FileNotFoundException("not found: " + key);
        }
        if (node == null) {
            throw new java.io.FileNotFoundException("null: " + key);
        }
        return node;
    }

    public static void main(String[] args) throws Exception {
        if (!"ABC".equals(require("abc"))) {
            throw new RuntimeException("require failed");
        }
        System.out.println("All tests passed!");
    }
}
