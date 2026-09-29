/**
 * Bug: a method declared to return a WRAPPER SUPERTYPE (e.g. Number, not
 * the exact wrapper Integer) whose body returns a bare primitive value
 * (e.g. "return Integer.parseInt(s);", an int) never got boxed at all -
 * coerce_stack_value()'s boxing check only recognized the EXACT wrapper
 * class or java.lang.Object as a legal box target, missing Number (and
 * any other reference supertype a boxed value widens to, e.g.
 * Comparable/Serializable). The bytecode returned the raw int with
 * ARETURN as if it were already a reference: VerifyError "Bad type on
 * operand stack ... Type integer ... not assignable to reference type"
 * at the areturn. Confirmed against gumdrop's own
 * ProtoFileParser.nextNumber(), whose body is exactly this shape
 * ("private Number nextNumber() { ...; return Integer.parseInt(s); }").
 */
public class PrimitiveReturnBoxedToWrapperSupertypeVerifyTest {

    private static Number toNumber(String s) {
        try {
            return Integer.parseInt(s);
        } catch (NumberFormatException e) {
            return Long.parseLong(s);
        }
    }

    public static void main(String[] args) {
        Number n = toNumber("42");
        if (!(n instanceof Integer) || n.intValue() != 42) {
            throw new RuntimeException("expected boxed Integer 42, got " + n);
        }
        Number big = toNumber("9999999999");
        if (!(big instanceof Long) || big.longValue() != 9999999999L) {
            throw new RuntimeException("expected boxed Long 9999999999, got " + big);
        }
    }
}
