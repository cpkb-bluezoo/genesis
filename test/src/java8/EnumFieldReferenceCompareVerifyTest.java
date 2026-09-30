/**
 * Bug: comparing two reference-typed (e.g. enum) INSTANCE FIELDS with
 * `==`/`!=` via their bare, unqualified names (implicit `this.field`)
 * used an INT comparison (IF_ICMPEQ/NE) instead of a REFERENCE
 * comparison (IF_ACMPEQ/NE) - VerifyError "Bad type on operand stack
 * ... is not assignable to integer". The `==`/`!=` codegen's own
 * reference-vs-primitive detection for an AST_IDENTIFIER operand only
 * ever checked `mg_local_is_ref()`, which only knows about tracked
 * LOCAL variables/parameters - it has no entry at all for a bare
 * identifier that's actually an instance or static FIELD, so it always
 * returned false for one, unlike the AST_METHOD_CALL/AST_FIELD_ACCESS/
 * AST_ARRAY_ACCESS/AST_CAST_EXPR branches, which already fell back to
 * the expression's own resolved `sem_type`. Matches gumdrop's own
 * QuicConnection.canAdoptVersion()'s "version != initialVersion", both
 * enum-typed instance fields compared via their bare names.
 */
public class EnumFieldReferenceCompareVerifyTest {
    enum Version { V1, V2, V3 }

    Version version = Version.V1;
    Version initialVersion = Version.V1;

    boolean matches() {
        if (version != initialVersion) {
            return false;
        }
        return true;
    }

    public static void main(String[] args) {
        EnumFieldReferenceCompareVerifyTest r = new EnumFieldReferenceCompareVerifyTest();
        if (!r.matches()) {
            throw new RuntimeException("expected matches() true");
        }
        r.version = Version.V2;
        if (r.matches()) {
            throw new RuntimeException("expected matches() false");
        }
    }
}
