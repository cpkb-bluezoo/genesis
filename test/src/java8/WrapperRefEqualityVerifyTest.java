/*
 * Per JLS 15.21, "==" / "!=" between two operands that are BOTH of
 * reference type - including two WRAPPER types, e.g. "Boolean.TRUE ==
 * someBooleanField" - is a REFERENCE comparison (object identity), not a
 * numeric one; unboxing only applies when exactly one side is a genuine
 * primitive. codegen_expr.c's binary-expression codegen unconditionally
 * auto-unboxed EACH operand independently based solely on that operand's
 * own wrapper-ness, unboxing BOTH sides of a wrapper-vs-wrapper "==" -
 * the separate is_ref_compare decision (which reads static sem_type, not
 * what was actually pushed) then still correctly chose IF_ACMPEQ/NE for
 * what were now two unboxed ints on the stack: VerifyError "Bad type on
 * operand stack ... not assignable to reference type".
 *
 * Confirmed against gumdrop's own Request.isRequestedSessionIdFromCookie()
 * (org.bluezoo.gumdrop.servlet): "Boolean.TRUE == sessionType".
 */
public class WrapperRefEqualityVerifyTest {
    Boolean sessionType = Boolean.TRUE;

    boolean isFromCookie() {
        return Boolean.TRUE == sessionType;
    }

    public static void main(String[] args) {
        WrapperRefEqualityVerifyTest r = new WrapperRefEqualityVerifyTest();
        if (!r.isFromCookie()) {
            throw new RuntimeException("expected true");
        }
        r.sessionType = Boolean.FALSE;
        if (r.isFromCookie()) {
            throw new RuntimeException("expected false");
        }
        System.out.println("WrapperRefEqualityVerifyTest passed!");
    }
}
