/*
 * Regression test: a boxed Boolean used directly as a ternary
 * condition (e.g. "(Boolean) value ? 1 : 0", the exact shape of
 * gumdrop's `FieldTable.writeValue()`: "buf.put((byte)
 * (((Boolean) value) ? 1 : 0));") used to leave a `java/lang/Boolean`
 * reference on the operand stack right where the ternary's own `ifeq`
 * branch requires an unboxed int:
 *
 *   java.lang.VerifyError: Bad type on operand stack
 *   Reason: Type 'java/lang/Boolean' (current frame, stack[0])
 *           is not assignable to integer
 *
 * Root cause: codegen_expr.c's AST_CONDITIONAL_EXPR (ternary) case
 * generated the condition's value and emitted `ifeq` directly, with no
 * unboxing step - unlike AST_IF_STMT in codegen_stmt.c, which already
 * had the matching "auto-unbox Boolean to boolean for if condition"
 * fix. Fixed by adding the identical unboxing call (Boolean.
 * booleanValue()) to the ternary's own condition handling.
 */
public class BoxedBooleanTernaryConditionVerifyTest {
    static byte pick(Object value) {
        return (byte) (((Boolean) value) ? 1 : 0);
    }

    public static void main(String[] args) {
        if (pick(Boolean.TRUE) != 1) {
            throw new RuntimeException("expected 1 for TRUE");
        }
        if (pick(Boolean.FALSE) != 0) {
            throw new RuntimeException("expected 0 for FALSE");
        }
        System.out.println("BoxedBooleanTernaryConditionVerifyTest passed!");
    }
}
