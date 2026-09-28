/*
 * Regression test: a narrowing primitive cast whose operand is itself a
 * compound expression - not a bare identifier or literal - e.g.
 * "(int) (pos + n)" where pos is long and n is int, used to produce a
 * classfile that failed JVM bytecode verification:
 *
 *   java.lang.VerifyError: Bad type on operand stack
 *   Reason: Type long_2nd (current frame, stack[k]) is not assignable to
 *   integer
 *
 * get_expression_type()'s AST_CAST_EXPR case in semantic.c resolves and
 * self-annotates the cast's own result type (from its target type node),
 * but never called get_expression_type() on the operand itself for the
 * general case - only when the operand is a lambda or method reference,
 * to bind it to the cast's target type. So an ordinary operand's own
 * sem_type was simply never computed during semantic analysis - nothing
 * else naturally visits it, since the cast's own result type is already
 * known independently from its target type node. codegen_expr.c's own
 * AST_CAST_EXPR codegen reads operand->sem_type directly to decide which
 * narrowing/widening conversion instruction the cast needs (e.g. l2i for
 * long-to-int) - and with it unset, silently defaulted to "assume int",
 * concluding no conversion was needed for a long-to-int cast and skipping
 * the l2i entirely. The result: the actual long value (from evaluating
 * "pos + n" with long/int operands, promoted to long per JLS 5.6.2) was
 * left on the stack and stored directly into an int local, which the
 * verifier correctly rejected. Fixed by having semantic.c's AST_CAST_EXPR
 * case call get_expression_type() on its operand unconditionally, so the
 * operand's own case (here AST_PARENTHESIZED, which already self-
 * annotates by propagating its inner expression's type) gets to run and
 * populate operand->sem_type before codegen ever reads it.
 */
public class CastOperandTypeResolutionVerifyTest {
    static int computeEnd(long pos, int n) {
        return (int) (pos + n);
    }

    public static void main(String[] args) {
        int end = computeEnd(10L, 5);
        if (end != 15) {
            throw new RuntimeException("expected 15, got " + end);
        }

        long big = 3_000_000_000L;
        int wrapped = (int) (big + 1);
        if (wrapped != (int) 3_000_000_001L) {
            throw new RuntimeException("expected wraparound narrowing to match javac, got " + wrapped);
        }

        System.out.println("All tests passed!");
    }
}
