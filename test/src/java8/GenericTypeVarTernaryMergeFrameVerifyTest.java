/*
 * Regression test: a ternary conditional expression whose own type
 * (JLS 15.25) is a bare, unbounded type variable - e.g. a generic
 * method "<T> T coalesce(T local, T fallback) { return local != null ?
 * local : fallback; }" - matching gumdrop's own
 * TlsConfig.coalesce(T, T). Used to throw at class-verification time:
 *
 *   java.lang.VerifyError: Inconsistent stackmap frames at branch target
 *   Reason: Type 'java/lang/Object' (current frame, stack[0]) is not
 *   assignable to null (stack map, stack[0])
 *
 * Root cause: codegen_expr.c's AST_CONDITIONAL_EXPR codegen records a
 * merge-point stackmap frame after generating both branches - since
 * mg->stackmap only ever reflects whichever branch was generated LAST
 * (the else branch), not a true merge of both incoming edges, it
 * corrects the frame using the ternary's own overall type
 * (expr->sem_type) when that type is a reference type. That correction
 * was gated on TYPE_CLASS/TYPE_ARRAY only - never TYPE_TYPEVAR - so a
 * ternary whose type is a bare, unresolved type variable (both operands
 * being of the method's own generic type T) fell through the gate
 * entirely, leaving whatever (possibly wrong) type the else branch's own
 * codegen had tracked for the merge point uncorrected.
 *
 * Fixed by including TYPE_TYPEVAR in the gate - type_to_descriptor()
 * already erases a type variable to its bound (or java.lang.Object if
 * unbounded), so no further special-casing was needed once the gate
 * itself covered this kind.
 */
public class GenericTypeVarTernaryMergeFrameVerifyTest {
    private static <T> T coalesce(T local, T fallback) {
        return local != null ? local : fallback;
    }

    public static void main(String[] args) {
        String a = coalesce("hello", "world");
        String b = coalesce(null, "world");

        if (!"hello".equals(a)) {
            throw new RuntimeException("expected hello, got " + a);
        }
        if (!"world".equals(b)) {
            throw new RuntimeException("expected world, got " + b);
        }

        Integer x = coalesce(Integer.valueOf(42), Integer.valueOf(0));
        Integer y = coalesce(null, Integer.valueOf(7));
        if (x.intValue() != 42) {
            throw new RuntimeException("expected 42, got " + x);
        }
        if (y.intValue() != 7) {
            throw new RuntimeException("expected 7, got " + y);
        }

        System.out.println("GenericTypeVarTernaryMergeFrameVerifyTest passed!");
    }
}
