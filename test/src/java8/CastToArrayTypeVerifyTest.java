/*
 * Regression test: a cast to an array type (`(byte[]) obj`) whose
 * result is then used somewhere requiring the real, narrower array
 * type - exactly gumdrop's own MessageParserTest shape:
 *
 *   Object[] ids = {..., new byte[] {9, 9}};
 *   ...
 *   if (ids[i] instanceof byte[]) {
 *       assertArrayEquals((byte[]) ids[i], (byte[]) got);
 *   }
 *
 * used to throw:
 *
 *   java.lang.VerifyError: Bad type on operand stack
 *   Type 'java/lang/Object' ... is not assignable to '[B'
 *
 * Root cause: codegen_expr.c's AST_CAST_EXPR codegen had a literal
 * `/* TODO: Handle array type descriptor *\/` gap for a cast whose
 * target type is an array (AST_ARRAY_TYPE) - target_class (used to
 * build the checkcast's constant-pool entry a few lines later) was
 * simply never set for this case, so the "if (target_class)" guard
 * silently skipped emitting ANY checkcast instruction at all - leaving
 * the operand's pre-cast type (e.g. Object, from an Object[] array
 * access) completely untouched, rejected the moment the result was
 * used somewhere requiring the real, narrower array type. Fixed by
 * resolving the target's array type (forcing resolution via
 * semantic_resolve_type() if some earlier pass hadn't already) and
 * building the full array descriptor ("[B", "[[I",
 * "[Ljava/lang/String;", ...) from it via type_to_descriptor() - per
 * JVMS 4.4.1, a CONSTANT_Class's name can be either a binary class name
 * or an array type descriptor, so this same "target_class" mechanism
 * already used for a plain class cast's checkcast works unchanged once
 * given the right value.
 */
import java.util.UUID;
import java.util.Arrays;

public class CastToArrayTypeVerifyTest {
    public static void main(String[] args) {
        Object[] ids = { Long.valueOf(7), "text-id", UUID.randomUUID(), new byte[] { 9, 9 } };
        boolean sawByteArray = false;
        for (int i = 0; i < ids.length; i++) {
            if (ids[i] instanceof byte[]) {
                byte[] b = (byte[]) ids[i];
                if (!Arrays.equals(b, new byte[] { 9, 9 })) {
                    throw new AssertionError("unexpected bytes: " + Arrays.toString(b));
                }
                sawByteArray = true;
            }
        }
        if (!sawByteArray) {
            throw new AssertionError("expected to see a byte[] element");
        }
        System.out.println("CastToArrayTypeVerifyTest passed!");
    }
}
