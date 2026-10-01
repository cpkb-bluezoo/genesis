import strswitchlib.ExternalStringConstants;

/*
 * Bug: a String switch's case label referencing a CLASSFILE-loaded (real
 * javac-compiled, referenced via -cp) String constant - e.g. gumdrop's own
 * "case HttpServletRequest.DIGEST_AUTH:" (jakarta.servlet's own interface,
 * an external dependency) - never matched anything at runtime, silently
 * always falling through to default, with no compile error. Root cause:
 * classfile_get_attribute_constant_value() (classfile.c) - which reads a
 * "static final" field's own ConstantValue attribute so a switch case
 * label referencing it can resolve to that value - only ever handled
 * CONSTANT_Integer/CONSTANT_Long pool entries (all an int-family switch's
 * case label could ever need), explicitly returning false for a
 * CONSTANT_String entry. A String switch's own case-label resolution
 * (semantic.c) had nowhere else to get the value from for a field with no
 * AST declarator (a classfile-loaded field has none). Fixed by adding a
 * sibling function, classfile_get_attribute_constant_value_string(), and a
 * matching const_str_value field on the field symbol, read the same way
 * const_value already was for int-family constants. Confirmed against
 * gumdrop's own HttpAuthenticationProvider.generateChallenge()'s
 * "switch (authMethod) { case HttpServletRequest.DIGEST_AUTH: ... }".
 */
public class StringSwitchExternalConstantTest {
    static String dispatch(String method) {
        switch (method) {
            case ExternalStringConstants.DIGEST_AUTH:
                return "digest";
            case ExternalStringConstants.BASIC_AUTH:
                return "basic";
            default:
                return "unknown";
        }
    }

    public static void main(String[] args) {
        String result = dispatch("DIGEST") + "," + dispatch("BASIC") + "," + dispatch("OTHER");
        if (!"digest,basic,unknown".equals(result)) {
            throw new RuntimeException("expected digest,basic,unknown but got " + result);
        }
        System.out.println("StringSwitchExternalConstantTest passed!");
    }
}
