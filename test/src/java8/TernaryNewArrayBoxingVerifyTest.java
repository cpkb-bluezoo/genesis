/*
 * Regression test: a ternary (conditional) expression with one branch a bare
 * `null` literal and the other an array-creation expression (`new byte[n]`)
 * used to produce a class file that failed JVM bytecode verification:
 *
 *   java.lang.VerifyError: Bad type on operand stack
 *   Reason: Type '[B' (current frame, stack[k]) is not assignable to integer
 *
 * get_expression_type()'s AST_NEW_ARRAY case in semantic.c computed and
 * returned the array's type correctly, but - unlike most of its sibling
 * cases - never wrote that type back onto the array-creation node itself
 * (expr->sem_type), the same "case never self-annotates" gap already found
 * for AST_FIELD_ACCESS and AST_ARRAY_ACCESS. This went unnoticed for plain
 * array creation (most callers use get_expression_type()'s return value
 * directly), but codegen_expr.c's ternary codegen reads a branch's own
 * sem_type off its node (via value_kind_and_class(), to decide whether the
 * branch needs a boxing/unboxing conversion to match the ternary's overall
 * type) - and with sem_type unset, that lookup fell through to a hardcoded
 * default of TYPE_INT for any unrecognized expression shape. Once the
 * ternary's own overall type is a reference type (as it always should be
 * for "null : new byte[n]"), coerce_stack_value() then "corrected" the
 * supposedly-int array-creation branch by splicing in a bogus
 * Integer.valueOf(I) call right after the real `new byte[n]` bytecode -
 * something the verifier correctly rejects, since the actual value on the
 * stack is a byte array, not an int. Fixed by self-annotating
 * expr->sem_type on the AST_NEW_ARRAY node like its siblings.
 */
public class TernaryNewArrayBoxingVerifyTest {
    static class Holder {
        byte[] data;
        Holder(boolean empty) {
            this.data = empty ? null : new byte[5];
        }
    }

    public static void main(String[] args) {
        Holder h1 = new Holder(true);
        Holder h2 = new Holder(false);
        if (h1.data != null) {
            throw new RuntimeException("expected null data for empty holder");
        }
        if (h2.data == null || h2.data.length != 5) {
            throw new RuntimeException("expected a 5-byte array for non-empty holder");
        }
        System.out.println("All tests passed!");
    }
}
