package stringswitchconst;

/*
 * Regression test: a String switch's case label referencing a named
 * String constant - either bare ("case OWN_CONST:", a same-class constant)
 * or qualified ("case StringConstants.BASIC:", from another type in the
 * same compilation batch) - never matched anything at runtime, silently
 * always falling through to default, with no compile error at all.
 * Matches the real-world shape of gumdrop's own
 * HttpAuthenticationProvider.generateChallenge()'s
 * "switch (authMethod) { case HttpServletRequest.DIGEST_AUTH: ... }".
 *
 * Root cause: semantic.c's switch-statement case-label processing already
 * had int-only machinery for resolving a named-constant case label (both
 * bare and qualified) to its actual value - added for INT switches
 * specifically (see SwitchQualifiedConstantCaseLabelVerifyTest.java in the
 * parent directory) - but nothing extended it to a STRING switch's case
 * labels: a literal case label ("case \"basic\":") already worked (handled
 * generically, by token type, not by resolving a named constant), but a
 * CONSTANT REFERENCE case label was only ever transformed into an
 * AST_LITERAL carrying an int value, regardless of the switch's actual
 * selector type. Fixed by adding a parallel resolution path
 * (resolve_named_string_constant()/resolve_qualified_constant_case_string_value())
 * used whenever the switch selector's type is java.lang.String, mirroring
 * the existing int-resolution functions' own symbol lookup exactly.
 *
 * A second, more subtle bug surfaced once resolution itself worked: the
 * transformed case_expr node's resolved value has to be stored in BOTH
 * data.leaf.name (interned) AND data.leaf.value.str_val/str_len - a
 * genuinely-parsed string literal token always carries both (see
 * ast_new_literal_from_lexer()'s identical dual assignment), and
 * codegen_string_switch()'s own case-value extraction reads ONLY
 * data.leaf.name, not value.str_val. Setting only value.str_val (the more
 * "obviously correct" field for a string value) left data.leaf.name NULL,
 * which codegen_string_switch() then fed straight to string_hashcode() -
 * not a crash, just silently computing hashCode(NULL) as 0 for every such
 * case and generating an ldc_w with constant pool index 0:
 * java.lang.VerifyError: "Illegal constant pool index 0".
 */
public class StringSwitchQualifiedConstantCaseLabelVerifyTest {
    private static final String BEARER = "Bearer";

    static String dispatchBare(String method) {
        switch (method) {
            case BEARER:
                return "bearer";
            default:
                return "unknown";
        }
    }

    static String dispatchQualified(String method) {
        switch (method) {
            case StringConstants.BASIC:
                return "basic";
            case StringConstants.DIGEST:
                return "digest";
            default:
                return "unknown";
        }
    }

    public static void main(String[] args) {
        String bareResult = dispatchBare("Bearer") + "," + dispatchBare("Other");
        if (!"bearer,unknown".equals(bareResult)) {
            throw new RuntimeException("expected bearer,unknown but got " + bareResult);
        }

        String qualifiedResult = dispatchQualified("Basic") + "," +
            dispatchQualified("Digest") + "," + dispatchQualified("Other");
        if (!"basic,digest,unknown".equals(qualifiedResult)) {
            throw new RuntimeException("expected basic,digest,unknown but got " + qualifiedResult);
        }

        System.out.println("StringSwitchQualifiedConstantCaseLabelVerifyTest passed!");
    }
}
