/*
 * A bare switch case label naming a constant brought in via a WILDCARD
 * static import ("import static genesislib.SocksAtypConsts.*;")
 * collapsed to match value 0 for every such case: resolve_named_int_constant()
 * and the switch statement's own duplicate-case-label-check loop (both
 * in semantic.c) never checked sem->static_imports at all - a
 * completely separate resolution mechanism from scope/inheritance,
 * already implemented for ordinary identifier EXPRESSIONS via
 * resolve_static_import_field(), just never wired into either
 * case-label resolver. Fixing that surfaced a SECOND, related gap: the
 * field symbol resolve_static_import_field() returns has its ->ast set
 * to the whole AST_FIELD_DECL (one declarator per comma-separated
 * name), not directly the AST_VAR_DECLARATOR the two callers expected -
 * needing the same declarator-unwrapping resolve_qualified_constant_case_value()
 * already had for its own (different) classpath-completion case.
 * Duplicate lookupswitch match values, VerifyError/ClassFormatError
 * "Bad lookupswitch instruction". Confirmed against gumdrop's own
 * SocksProtocolHandler.handleSOCKS5Request()'s "switch (atyp)", whose
 * three case labels are all brought in via "import static
 * org.bluezoo.gumdrop.socks.SocksConstants.*;".
 */
import static genesislib.SocksAtypConsts.*;

public class StaticImportSwitchCaseLabelTest {
    static String classify(byte atyp) {
        switch (atyp) {
            case ATYP_IPV4:
                return "IPV4";
            case ATYP_DOMAINNAME:
                return "DOMAINNAME";
            case ATYP_IPV6:
                return "IPV6";
            default:
                return "UNKNOWN";
        }
    }

    public static void main(String[] args) {
        if (!"IPV4".equals(classify((byte) 1))) {
            System.out.println("FAILED: expected IPV4, got " + classify((byte) 1));
            System.exit(1);
        }
        if (!"DOMAINNAME".equals(classify((byte) 3))) {
            System.out.println("FAILED: expected DOMAINNAME, got " + classify((byte) 3));
            System.exit(1);
        }
        if (!"IPV6".equals(classify((byte) 4))) {
            System.out.println("FAILED: expected IPV6, got " + classify((byte) 4));
            System.exit(1);
        }
        if (!"UNKNOWN".equals(classify((byte) 99))) {
            System.out.println("FAILED: expected UNKNOWN, got " + classify((byte) 99));
            System.exit(1);
        }
        System.out.println("StaticImportSwitchCaseLabelTest passed!");
    }
}
