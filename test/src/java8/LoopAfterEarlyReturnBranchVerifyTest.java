import java.util.Arrays;
import java.util.List;

/*
 * Regression test: a loop (while/for/do-while/for-each) whose own body's
 * last statement is a plain statement (not itself a return/throw/break)
 * used to execute only ONCE - falling straight through into whatever
 * code follows the loop's enclosing branch - whenever the loop lived
 * inside an "else if" branch of an if/else-if chain whose EARLIER
 * sibling branch ends in a return. Matches the real-world shape of
 * gumdrop's own AMQP FieldTable.valueSize(): a chain of
 * "if (value instanceof X) return N;" branches, with a final
 * "else if (value instanceof List) { <for-each summing element sizes> }"
 * branch - silently undercounting the array's encoded size (only the
 * first element's size was ever added) and later crashing encode() with
 * a BufferOverflowException once boxing was fixed enough for genesis to
 * reach this code path at all.
 *
 * Root cause: codegen_stmt.c's loop constructs (AST_WHILE_STMT,
 * AST_FOR_STMT, AST_DO_STMT, and the Iterator-based AST_ENHANCED_FOR_STMT)
 * each decide whether to skip the loop's own back-edge goto (because the
 * body already ends in an unconditional jump) by checking mg->last_opcode
 * right after generating the body. But mg->last_opcode is a manually
 * tracked field, only set by specific constructs (return/throw/catch/if
 * handling) - a plain statement like "total += 1;" never touches it. An
 * if-WITHOUT-else always resets mg->last_opcode = 0 right after its own
 * codegen (since the false path is always live) - but only AFTER its
 * entire body (which may itself contain the loop) has already been
 * generated and made its own, premature decision. So when an earlier
 * sibling branch in the SAME if/else-if chain ends in a return (setting
 * mg->last_opcode to e.g. OP_IRETURN) and the loop lives inside a LATER,
 * still-un-reset branch, the loop's own body_ends_with_jump check sees
 * that stale terminal opcode and wrongly skips its back-edge - turning
 * the loop into a single iteration that falls straight through. Fixed by
 * resetting mg->last_opcode = 0 immediately before generating each
 * loop's own body, mirroring the identical, pre-existing reset already
 * used before a catch block's own body for the same reason.
 */
public class LoopAfterEarlyReturnBranchVerifyTest {
    static int whileSum(Object x, int n) {
        if (x == null) {
            return -1;
        } else if (x instanceof String) {
            int total = 0;
            int i = 0;
            while (i < n) {
                total += 1;
                i++;
            }
            return total;
        }
        return -2;
    }

    static int forSum(Object x, int n) {
        if (x == null) {
            return -1;
        } else if (x instanceof String) {
            int total = 0;
            for (int i = 0; i < n; i++) {
                total += 1;
            }
            return total;
        }
        return -2;
    }

    static int doWhileSum(Object x, int n) {
        if (x == null) {
            return -1;
        } else if (x instanceof String) {
            int total = 0;
            int i = 0;
            do {
                total += 1;
                i++;
            } while (i < n);
            return total;
        }
        return -2;
    }

    static int forEachSum(Object x, List<?> list) {
        if (x == null) {
            return -1;
        } else if (x instanceof String) {
            int total = 0;
            for (Object element : list) {
                total += 1;
            }
            return total;
        }
        return -2;
    }

    public static void main(String[] args) {
        if (whileSum("x", 4) != 4) {
            throw new RuntimeException("whileSum: expected 4, got " + whileSum("x", 4));
        }
        if (forSum("x", 4) != 4) {
            throw new RuntimeException("forSum: expected 4, got " + forSum("x", 4));
        }
        if (doWhileSum("x", 4) != 4) {
            throw new RuntimeException("doWhileSum: expected 4, got " + doWhileSum("x", 4));
        }
        List<Object> list = Arrays.<Object>asList(1, 2, 3, "four");
        if (forEachSum("x", list) != 4) {
            throw new RuntimeException("forEachSum: expected 4, got " + forEachSum("x", list));
        }
        System.out.println("LoopAfterEarlyReturnBranchVerifyTest passed!");
    }
}
