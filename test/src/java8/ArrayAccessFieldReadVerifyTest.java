/*
 * Regression test: reading a field off an array-element expression
 * (`values[i].code`, not a bare identifier receiver) inside an `int`
 * comparison - exactly gumdrop's own NamedGroup.fromCode() shape:
 *
 *   for (int i = 0; i < values.length; i++) {
 *       if (values[i].code == code) {
 *           return values[i];
 *       }
 *   }
 *
 * used to throw:
 *
 *   java.lang.VerifyError: Bad type on operand stack
 *   Type 'java/lang/Object' (current frame, stack[0]) is not assignable
 *   to integer
 *
 * Root cause: get_expression_type()'s AST_ARRAY_ACCESS case
 * (semantic.c) computes an array access's element type correctly, but
 * - like several sibling cases fixed earlier this session
 * (AST_FIELD_ACCESS, AST_NEW_ARRAY, ...) - never wrote it back onto
 * expr->sem_type. codegen_field_access()'s general-case field read
 * (codegen_expr.c) reads its receiver's own sem_type to know the field's
 * real declared type; finding it NULL for an AST_ARRAY_ACCESS receiver,
 * it silently fell back to treating the field as `Ljava/lang/Object;`
 * regardless of its real type - so `values[i].code` (an `int` field)
 * pushed a bogus "Object" stackmap entry for what the actual GETFIELD
 * instruction genuinely produces as an int, rejected the moment that
 * value was used somewhere requiring a real int (here, an int
 * comparison). Fixed by self-annotating expr->sem_type in both the
 * single- and multi-dimensional branches of AST_ARRAY_ACCESS, matching
 * every sibling case in the same function.
 */
public class ArrayAccessFieldReadVerifyTest {
    enum Group {
        A(10), B(20), C(30);
        final int code;
        Group(int code) {
            this.code = code;
        }
    }

    static Group fromCode(int code) {
        Group[] values = Group.values();
        for (int i = 0; i < values.length; i++) {
            if (values[i].code == code) {
                return values[i];
            }
        }
        return null;
    }

    public static void main(String[] args) {
        if (fromCode(20) != Group.B) {
            throw new AssertionError("expected B");
        }
        if (fromCode(99) != null) {
            throw new AssertionError("expected null for unknown code");
        }
        System.out.println("ArrayAccessFieldReadVerifyTest passed!");
    }
}
