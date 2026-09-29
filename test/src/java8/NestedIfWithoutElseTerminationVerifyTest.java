/*
 * Regression test: a void method's final if/else-if statement, where
 * the THEN branch is itself a block whose only (last) statement is a
 * nested if-WITHOUT-else that ends in a throw (e.g.
 * "if (A) { if (B) throw ...; } else if (C) throw ...;"), followed by
 * more code after the whole if/else-if - used to generate:
 *
 *   java.lang.VerifyError: Control flow falls through code end
 *
 * matching gumdrop's own AMQP 1.0 message parser,
 * MessageParser.checkOrder(long), which has exactly this shape (a
 * second if/else-if after an initial if/else-if chain, ending with a
 * plain "stage = s;" assignment - see that method for the real-world
 * original).
 *
 * Root cause: codegen_stmt.c's AST_IF_STMT case decided whether its own
 * then/else branch "ends with a return" (and so needs no goto past the
 * other branch, or - if BOTH branches end this way - marks the whole
 * if/else as itself terminating) by inspecting the single, literal LAST
 * BYTE already emitted into the bytecode array, checking whether it's
 * one of the return/throw opcodes. This is a false positive whenever
 * the branch's own last statement is itself an if-WITHOUT-else whose
 * body ends in a throw/return: that inner if-without-else's own FALSE
 * path needs no bytecode of its own when there's nothing left in its
 * enclosing block after it (control simply falls through to whatever
 * follows the OUTER if/else-if in the surrounding scope) - so the
 * literal last emitted byte is still that inner throw/return's opcode,
 * even though the inner if-without-else, and therefore the outer
 * branch containing it, does NOT unconditionally terminate. This
 * false-positive "then/else branch terminates" conclusion skipped
 * emitting the goto past the other branch and could ultimately mark
 * the whole if/else as terminating when it wasn't, leaving the method
 * with no final return at all once its own last statement was reached
 * only via that non-terminating path.
 *
 * Fixed by using mg->last_opcode (a semantic "does this construct
 * genuinely, unconditionally terminate" flag - already correctly reset
 * to 0 by an if-without-else's own codegen, precisely because its false
 * path doesn't terminate) instead of the raw last-emitted-byte
 * inspection, mirroring the same mg->last_opcode-based termination
 * check already used by every loop construct in this file. A reset to 0
 * immediately before generating each of the then/else branches was also
 * added, so a branch whose own last statement is a plain, non-
 * terminating statement (which never touches mg->last_opcode itself)
 * doesn't inherit stale state left over from whatever code preceded the
 * if-statement.
 */
public class NestedIfWithoutElseTerminationVerifyTest {
    static int stage = 0;

    static void checkOrder(int s) {
        if (s == 100) {
            if (stage > 100) {
                throw new RuntimeException("late");
            }
        } else if (s <= stage) {
            throw new RuntimeException("out of order");
        }
        stage = s;
    }

    public static void main(String[] args) {
        checkOrder(1);
        if (stage != 1) {
            throw new RuntimeException("expected stage=1, got " + stage);
        }
        checkOrder(2);
        if (stage != 2) {
            throw new RuntimeException("expected stage=2, got " + stage);
        }
        checkOrder(100);
        if (stage != 100) {
            throw new RuntimeException("expected stage=100, got " + stage);
        }

        boolean threw = false;
        try {
            checkOrder(1);
        } catch (RuntimeException e) {
            threw = true;
        }
        if (!threw) {
            throw new RuntimeException("expected an out-of-order exception");
        }

        System.out.println("NestedIfWithoutElseTerminationVerifyTest passed!");
    }
}
