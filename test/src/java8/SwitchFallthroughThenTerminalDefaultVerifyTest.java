/**
 * Bug: a void method whose last statement is a switch over an enum, where
 * an EARLIER case's body has code but ends without return/break/throw -
 * deliberately falling through into the next case (which does terminate) -
 * and every case, including a `default: throw ...` that is physically
 * last, terminates one way or another. The switch as a whole therefore
 * never falls through to "after the switch", so the compiler must not
 * append a trailing `return` there.
 *
 * codegen_stmt.c's AST_SWITCH_STMT case previously required EVERY
 * individual case body (that emitted any code) to itself end in
 * return/throw before treating the whole switch as terminating - so a
 * case that legitimately falls through with no return of its own (like
 * `case A` below) made it reset mg->last_opcode to 0, and the enclosing
 * method epilogue then appended a spurious `return` right after the
 * switch's own final `athrow` (the default case). That trailing `return`
 * is genuinely unreachable (nothing branches to it, and the instruction
 * before it doesn't fall through into it either) with no stack map frame
 * of its own - VerifyError: "Expecting a stack map frame".
 */
public class SwitchFallthroughThenTerminalDefaultVerifyTest {
    enum Phase { A, B, C }

    static int result;

    static void process(Phase phase, String token) {
        switch (phase) {
            case A:
                if (token.length() > 0) {
                    result = 1;
                    return;
                }
                // fall through
            case B:
                result = 2;
                return;
            case C:
                result = 3;
                return;
            default:
                throw new IllegalStateException("unreachable");
        }
    }

    public static void main(String[] args) {
        result = 0;
        process(Phase.A, "");
        if (result != 2) {
            throw new RuntimeException("expected fallthrough from A to B (result=2), got " + result);
        }

        result = 0;
        process(Phase.A, "x");
        if (result != 1) {
            throw new RuntimeException("expected case A's own return (result=1), got " + result);
        }

        result = 0;
        process(Phase.C, "");
        if (result != 3) {
            throw new RuntimeException("expected case C (result=3), got " + result);
        }
    }
}
