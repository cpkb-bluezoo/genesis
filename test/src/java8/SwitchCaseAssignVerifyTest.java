import java.nio.file.AccessMode;

/*
 * Regression test: a local variable declared before a switch statement
 * and assigned in every case (each ending in break), then used
 * afterward, used to produce a class file that failed JVM bytecode
 * verification:
 *
 *   java.lang.VerifyError: Inconsistent stackmap frames at branch target N
 *   Reason: Type top (current frame, locals[k]) is not assignable to
 *   '...' (stack map, locals[k])
 *
 * codegen_stmt.c's AST_SWITCH_STMT handling records each case label's
 * stack-map frame using whatever mg->stackmap happens to be tracking at
 * that point in *code-generation* order - i.e. whatever the previous case
 * in AST order left behind - rather than the state actually live at that
 * label (which, absent a fallthrough, is reached *only* from the
 * lookupswitch dispatch itself, at switch entry). So the first case's
 * declared frame was correct (nothing preceded it), but every case after
 * it inherited whatever locals the previous case happened to assign,
 * making a not-yet-assigned local look assigned from the second case
 * onward - the exact same "join point framed from stale state" bug
 * already fixed for finally handlers, if/else, try/catch, and
 * synchronized statements, here in switch case labels.
 * See genesis history for details (search "switch_entry_state" in
 * codegen_stmt.c).
 */
public class SwitchCaseAssignVerifyTest {
    static String classify(AccessMode mode) {
        String needed;
        switch (mode) {
            case READ:
                needed = "R";
                break;
            case WRITE:
                needed = "W";
                break;
            default:
                needed = "X";
                break;
        }
        return needed + "!";
    }

    public static void main(String[] args) {
        if (!"R!".equals(classify(AccessMode.READ))) {
            throw new RuntimeException("READ failed");
        }
        if (!"W!".equals(classify(AccessMode.WRITE))) {
            throw new RuntimeException("WRITE failed");
        }
        if (!"X!".equals(classify(AccessMode.EXECUTE))) {
            throw new RuntimeException("EXECUTE failed");
        }
        System.out.println("All tests passed!");
    }
}
