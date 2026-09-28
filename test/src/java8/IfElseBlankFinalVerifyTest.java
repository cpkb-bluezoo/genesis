/*
 * Regression test: a blank final (or any pre-existing local) assigned in
 * every live branch of an if/else-if/.../else chain, where the final else
 * branch throws instead of assigning it, used to produce a class file that
 * failed JVM bytecode verification with:
 *
 *   java.lang.VerifyError: Bad local variable type
 *   Reason: Type top (current frame, locals[N]) is not assignable to
 *   reference type
 *
 * codegen_stmt.c's AST_IF_STMT handling records the stack-map frame at the
 * join point after the if/else using whatever mg->stackmap happens to be
 * tracking once the else branch finishes generating. For an else branch
 * that itself throws (rather than falling through to the join point), the
 * *only* live edge into the join point is the goto emitted at the end of
 * the then branch - but when the else branch is itself a nested if/else
 * (from "else if"), generating it manipulates mg->stackmap for its own
 * internal framing (e.g. resetting the local back to Top right before
 * recording its own throw-handler's entry frame), leaving the tracked
 * state wrong for the *outer* join point by the time control returns here.
 * The recorded frame ended up claiming the local was still unassigned
 * (Top), while the actual bytecode reaching that point (via the then
 * branch's goto) has it assigned to a real reference type.
 * See genesis history for details (search "then_exit_state" in
 * codegen_stmt.c).
 */
public class IfElseBlankFinalVerifyTest {

    static String pick(String syntax, String value) {
        final String result;
        if ("upper".equalsIgnoreCase(syntax)) {
            result = value.toUpperCase();
        } else if ("lower".equalsIgnoreCase(syntax)) {
            result = value.toLowerCase();
        } else {
            throw new UnsupportedOperationException("Syntax: " + syntax);
        }
        return result + "!";
    }

    public static void main(String[] args) {
        if (!"ABC!".equals(pick("upper", "abc"))) {
            throw new RuntimeException("upper branch failed");
        }
        if (!"xyz!".equals(pick("lower", "XYZ"))) {
            throw new RuntimeException("lower branch failed");
        }
        System.out.println("All tests passed!");
    }
}
