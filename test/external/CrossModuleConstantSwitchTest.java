/*
 * A bare switch case label naming a "static final int" constant
 * INHERITED from a SUPERCLASS compiled in a SEPARATE genesis invocation
 * (a different compile batch/module - see genesislib/ScopeConstants.java's
 * own comment) collapsed to match value 0 for every such case: genesis's
 * own classwriter never emitted a ConstantValue attribute for a "static
 * final" field (relying on <clinit> instead, which works for ordinary
 * runtime use but leaves nothing for a DIFFERENT compile batch's own
 * compile-time constant folding to read), and the field lookup itself
 * only ever checked the current class's own members and implemented
 * interfaces, never walking the superclass chain. Duplicate lookupswitch
 * match values, VerifyError/ClassFormatError "Bad lookupswitch
 * instruction". Confirmed against gumdrop's own test-only
 * MockPageContext, whose inherited "case PAGE_SCOPE:"/"case
 * REQUEST_SCOPE:"/etc. (from jakarta.servlet.jsp.PageContext, itself
 * gumdrop's own "core" module source, compiled separately from the
 * "servlet" test module referencing it) are exactly this shape.
 */
import genesislib.ScopeConstants;

public class CrossModuleConstantSwitchTest extends ScopeConstants {
    static String describe(int scope) {
        switch (scope) {
            case PAGE_SCOPE:
                return "PAGE";
            case REQUEST_SCOPE:
                return "REQUEST";
            case SESSION_SCOPE:
                return "SESSION";
            case APPLICATION_SCOPE:
                return "APPLICATION";
            default:
                return "UNKNOWN";
        }
    }

    public static void main(String[] args) {
        if (!"PAGE".equals(describe(1))) {
            System.out.println("FAILED: expected PAGE, got " + describe(1));
            System.exit(1);
        }
        if (!"REQUEST".equals(describe(2))) {
            System.out.println("FAILED: expected REQUEST, got " + describe(2));
            System.exit(1);
        }
        if (!"SESSION".equals(describe(3))) {
            System.out.println("FAILED: expected SESSION, got " + describe(3));
            System.exit(1);
        }
        if (!"APPLICATION".equals(describe(4))) {
            System.out.println("FAILED: expected APPLICATION, got " + describe(4));
            System.exit(1);
        }
        if (!"UNKNOWN".equals(describe(99))) {
            System.out.println("FAILED: expected UNKNOWN, got " + describe(99));
            System.exit(1);
        }
        System.out.println("CrossModuleConstantSwitchTest passed!");
    }
}
