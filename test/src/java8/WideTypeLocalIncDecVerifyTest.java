/*
 * Regression test: pre/post increment and decrement (++/--) on a local
 * variable of a category-2 (wide, 2-stack-word) type - long or double -
 * or a float, used to generate wrong bytecode. Root cause: the local-
 * variable increment/decrement codegen unconditionally used the JVM's
 * iinc/iload opcodes, which only ever operate on a single 32-bit
 * int-categorized local slot - there is no wide equivalent. For a
 * long/double local this silently corrupted the value and left the
 * slot's own stackmap-tracked type (long/double) out of sync with the
 * int-only iload/iinc that was actually emitted:
 *
 *   java.lang.VerifyError: Bad local variable type
 *   Reason: Type long (current frame, locals[N]) is not assignable to
 *           integer
 *
 * matching gumdrop's own AMQP 1.0 decoder,
 * Amqp1Decoder.readList()/readMap()/readArray(), each of which loops
 * "for (long i = 0; i < count; i++) { ... }".
 *
 * A second, related gap surfaced once the increment/decrement itself
 * was fixed: a for-loop's own update-clause codegen (codegen_stmt.c,
 * AST_FOR_STMT) unconditionally emitted a single-word OP_POP to discard
 * the update expression's value - correct for an int/float/reference
 * update clause, but wrong once "i++" on a long/double loop variable
 * correctly started leaving a 2-word value behind, corrupting the rest
 * of the stack (VerifyError: "Bad type on operand stack", a stray
 * long's second half left on the stack). Fixed by tracking the actual
 * stack-depth delta and using OP_POP2 when it's 2 or more, mirroring
 * the identical, already-correct logic in AST_EXPR_STMT's own pop-the-
 * result handling.
 */
public class WideTypeLocalIncDecVerifyTest {
    public static void main(String[] args) {
        long l = 5;
        if (l++ != 5 || l != 6) throw new RuntimeException("long post++ failed: " + l);
        if (++l != 7 || l != 7) throw new RuntimeException("long ++pre failed: " + l);
        if (l-- != 7 || l != 6) throw new RuntimeException("long post-- failed: " + l);
        if (--l != 5 || l != 5) throw new RuntimeException("long --pre failed: " + l);

        double d = 5.5;
        if (d++ != 5.5 || d != 6.5) throw new RuntimeException("double post++ failed: " + d);
        if (++d != 7.5 || d != 7.5) throw new RuntimeException("double ++pre failed: " + d);
        if (d-- != 7.5 || d != 6.5) throw new RuntimeException("double post-- failed: " + d);
        if (--d != 5.5 || d != 5.5) throw new RuntimeException("double --pre failed: " + d);

        float f = 2.5f;
        if (f++ != 2.5f || f != 3.5f) throw new RuntimeException("float post++ failed: " + f);
        if (++f != 4.5f || f != 4.5f) throw new RuntimeException("float ++pre failed: " + f);
        if (f-- != 4.5f || f != 3.5f) throw new RuntimeException("float post-- failed: " + f);
        if (--f != 2.5f || f != 2.5f) throw new RuntimeException("float --pre failed: " + f);

        StringBuilder sb = new StringBuilder();
        for (long i = 0; i < 3; i++) {
            sb.append(i);
        }
        if (!"012".equals(sb.toString())) {
            throw new RuntimeException("for-loop with long induction variable failed: " + sb);
        }

        StringBuilder sb2 = new StringBuilder();
        for (double x = 0; x < 3; x++) {
            sb2.append((long) x);
        }
        if (!"012".equals(sb2.toString())) {
            throw new RuntimeException("for-loop with double induction variable failed: " + sb2);
        }

        System.out.println("WideTypeLocalIncDecVerifyTest passed!");
    }
}
