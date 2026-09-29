/**
 * Bug: a chained/nested assignment used as a VALUE, e.g. the inner
 * "h = compute()" in "hex = h = compute();" (the classic double-checked-
 * locking idiom), was treated as a primitive int needing Integer.valueOf()
 * boxing before being stored into the OUTER assignment's reference-typed
 * target - because nothing ever determined an AST_ASSIGNMENT_EXPR node's
 * own type when it appears as another expression's operand, silently
 * falling through to a "default to int" fallback. VerifyError: "Bad type
 * on operand stack ... not assignable to integer" (Integer.valueOf(int)'s
 * own parameter slot, given an object reference instead of an int).
 * Confirmed against gumdrop's own TraceId.toHexString(), whose double-
 * checked-locking "hex = h = ByteArrays.toHexString(bytes);" is exactly
 * this shape.
 */
public class ChainedAssignmentReferenceTypeVerifyTest {
    private String hex;

    private String compute() {
        return "abc123";
    }

    public String toHexString() {
        String h = hex;
        if (h == null) {
            synchronized (this) {
                h = hex;
                if (h == null) {
                    hex = h = compute();
                }
            }
        }
        return h;
    }

    public static void main(String[] args) {
        ChainedAssignmentReferenceTypeVerifyTest t = new ChainedAssignmentReferenceTypeVerifyTest();
        String result = t.toHexString();
        if (!"abc123".equals(result)) {
            throw new RuntimeException("expected abc123, got " + result);
        }
        if (!"abc123".equals(t.toHexString())) {
            throw new RuntimeException("second call should still return abc123");
        }
    }
}
