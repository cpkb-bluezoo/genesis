/*
 * Regression test: a `new Type[]{...}` array-initializer literal nested
 * as one ELEMENT of an OUTER array initializer - exactly gumdrop's own
 * MessageParserTest shape:
 *
 *   Object[] ids = { Long.valueOf(7), "text-id", new byte[] { 9, 9 } };
 *
 * used to fail at compile time with:
 *
 *   codegen: array initializer without target type
 *   error: code generation failed for: ...
 *
 * Root cause: codegen_new_array()'s "new Type[]{...}" handling
 * delegates to codegen_array_init(), which requires the initializer's
 * own sem_type to already be populated by semantic.c's AST_NEW_ARRAY
 * handling in get_expression_type() - reliably true when the whole
 * expression is itself visited by that function directly, but NOT when
 * it appears nested as one element of an OUTER array initializer (the
 * outer initializer's own element-binding pass doesn't recurse through
 * that same AST_NEW_ARRAY-visiting path for each element). Fixed by
 * forcing resolution (calling get_expression_type() on the whole
 * AST_NEW_ARRAY expression) right before delegating, whenever the
 * initializer's own sem_type isn't already set to a real array type.
 */
public class NestedArrayLiteralAsElementVerifyTest {
    public static void main(String[] args) {
        Object[] mixed = { Integer.valueOf(1), "two", new byte[] { 3, 4 } };
        if (mixed.length != 3) {
            throw new AssertionError("expected 3 elements");
        }
        if (!(mixed[2] instanceof byte[])) {
            throw new AssertionError("expected a byte[] at index 2");
        }
        byte[] b = (byte[]) mixed[2];
        if (b[0] != 3 || b[1] != 4) {
            throw new AssertionError("unexpected bytes");
        }
        System.out.println("NestedArrayLiteralAsElementVerifyTest passed!");
    }
}
