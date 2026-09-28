import java.nio.file.AccessMode;

/*
 * Regression test: a switch statement whose selector is an enum-typed
 * expression that isn't a bare identifier - e.g. an array element,
 * `switch (modes[i])` - used to produce a class file that failed JVM
 * bytecode verification:
 *
 *   java.lang.VerifyError: Bad type on operand stack
 *   Reason: Type '<EnumType>' (current frame, stack[0]) is not assignable
 *   to integer
 *
 * codegen_stmt.c's AST_SWITCH_STMT handling calls Enum.ordinal() on the
 * selector before emitting the lookupswitch, but only when it can tell the
 * selector is enum-typed via selector->sem_type. That works for
 * `switch (someLocal)` because get_expression_type()'s AST_IDENTIFIER case
 * stores its resolved type back onto the node it resolved. It does NOT
 * work for `switch (modes[i])`, because get_expression_type()'s
 * AST_ARRAY_ACCESS case never stores its result back onto the node either
 * - so selector->sem_type stayed NULL, is_enum_switch was (wrongly) false,
 * ordinal() was never called, and the raw enum reference was fed straight
 * into lookupswitch, which needs an int.
 *
 * Semantic analysis's own switch-statement checker already computes the
 * right answer and stores it - just on the wrong node (the switch
 * statement itself, not the selector expression) - so the fix falls back
 * to that rather than fixing every expression shape's self-annotation
 * gap individually.
 * See genesis history for details (search "enum_switch_type" in
 * codegen_stmt.c).
 */
public class EnumSwitchArrayAccessTest {
    static String classify(AccessMode[] modes) {
        String result = "";
        for (int i = 0; i < modes.length; i++) {
            switch (modes[i]) {
                case READ:
                    result += "R";
                    break;
                case WRITE:
                    result += "W";
                    break;
                default:
                    result += "X";
                    break;
            }
        }
        return result;
    }

    public static void main(String[] args) {
        AccessMode[] modes = { AccessMode.READ, AccessMode.WRITE, AccessMode.EXECUTE };
        String result = classify(modes);
        if (!"RWX".equals(result)) {
            throw new RuntimeException("expected RWX, got " + result);
        }
        System.out.println("All tests passed!");
    }
}
