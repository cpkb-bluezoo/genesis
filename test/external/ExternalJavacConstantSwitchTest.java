/*
 * A bare switch case label naming a "static final int" constant
 * INHERITED from a SUPERCLASS compiled by real javac (not genesis, and
 * not an interface - see javaclib/ScopeConstants.java's own comment for
 * why that matters) collapsed to match value 0 for every such case:
 * neither the field lookup (which only ever checked the current class's
 * own members and implemented interfaces, never walking the superclass
 * chain) nor the value resolution (which only ever read an AST
 * declarator's initializer expression - nonexistent for a classfile-
 * loaded field) handled this shape. Duplicate lookupswitch match values,
 * VerifyError/ClassFormatError "Bad lookupswitch instruction". Confirmed
 * against gumdrop's own test-only MockPageContext, whose inherited
 * "case PAGE_SCOPE:"/"case REQUEST_SCOPE:"/etc. (from
 * javax.servlet.jsp.PageContext, part of the real servlet API JAR) are
 * exactly this shape.
 */
import javaclib.ScopeConstants;

public class ExternalJavacConstantSwitchTest extends ScopeConstants {
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
        System.out.println("ExternalJavacConstantSwitchTest passed!");
    }
}
