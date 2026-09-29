/**
 * Sibling bug to SwitchLocalOnlyInOneCaseVerifyTest, but in the SEPARATE
 * codegen path used for a String selector (codegen_string_switch(), which
 * lowers to hashCode()+equals() dispatch per Java 7+ string-switch rules).
 * That function has its own, independent exit-frame recording and did not
 * share the fix applied to the general (enum/int) AST_SWITCH_STMT case:
 * every case here ends in `break` to a SHARED exit point, but only the
 * case that declares its own local left that local's real type in the
 * live stackmap state by the time codegen reached the shared exit, so the
 * ONE recorded frame there was wrong for every other case's own break.
 * VerifyError: "Inconsistent stackmap frames ... not assignable" the
 * moment a different case's break reached that target. Confirmed against
 * gumdrop's own ProtoFileParser.parseMessage(), whose
 * `switch (tok) { case "option": ...; break; ... case "map":
 * FieldDescriptor mapField = ...; break; ... }` is exactly this shape.
 */
public class StringSwitchLocalOnlyInOneCaseVerifyTest {
    static int seen;

    static void dispatch(String tok) {
        switch (tok) {
            case "a":
                seen = 1;
                break;
            case "b":
                seen = 2;
                break;
            case "map":
                Integer boxed = Integer.valueOf(tok.length());
                seen = boxed;
                break;
            default:
                seen = -1;
        }
    }

    public static void main(String[] args) {
        dispatch("a");
        if (seen != 1) {
            throw new RuntimeException("expected seen=1 for \"a\", got " + seen);
        }
        dispatch("b");
        if (seen != 2) {
            throw new RuntimeException("expected seen=2 for \"b\", got " + seen);
        }
        dispatch("map");
        if (seen != "map".length()) {
            throw new RuntimeException("expected seen=" + "map".length() + " for \"map\", got " + seen);
        }
        dispatch("nope");
        if (seen != -1) {
            throw new RuntimeException("expected seen=-1 for default, got " + seen);
        }
    }
}
