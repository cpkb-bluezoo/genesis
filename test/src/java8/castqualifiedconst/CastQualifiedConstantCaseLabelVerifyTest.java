package castqualifiedconst;

/*
 * Regression test: a qualified static-final-long constant, narrowed by
 * an explicit cast, used as a switch case label - e.g.
 * "case (int) Descriptors.OPEN:" - matching gumdrop's own AMQP 1.0
 * codec, PerformativeCodec.decode(), which switches on
 * "(int) descriptor" with case labels like
 * "case (int) Performative.DESCRIPTOR_OPEN:" (DESCRIPTOR_OPEN declared
 * as a "long" constant, since AMQP 1.0 descriptor codes are decoded as
 * longs off the wire but only ever take small values in practice). This
 * used to compile silently but generate a broken lookupswitch with
 * every case's match key read as 0:
 *
 *   java.lang.VerifyError: Bad lookupswitch instruction
 *
 * Root cause: semantic.c's switch-statement case-label processing had a
 * branch for a bare qualified constant (AST_FIELD_ACCESS, e.g.
 * "case Type.CONSTANT:", fixed in an earlier bug) but none at all for
 * that SAME shape wrapped in a narrowing cast (AST_CAST_EXPR, e.g.
 * "case (int) Type.CONSTANT:") - needed whenever the constant's own
 * declared type is wider than the switch's selector type. Fixed by
 * extracting the existing AST_FIELD_ACCESS case-label resolution logic
 * into a shared helper, resolve_qualified_constant_case_value(), and
 * adding a new AST_CAST_EXPR branch that unwraps to the cast's operand,
 * resolves it the same way, and narrows the result to a plain (32-bit)
 * int the same way a real Java (int) cast of a long constant would.
 */
public class CastQualifiedConstantCaseLabelVerifyTest {
    static String name(long descriptor) {
        switch ((int) descriptor) {
            case (int) Descriptors.OPEN:
                return "open";
            case (int) Descriptors.CLOSE:
                return "close";
            case (int) Descriptors.BEGIN:
                return "begin";
            default:
                return "unknown";
        }
    }

    public static void main(String[] args) {
        String result = name(0x10) + "," + name(0x18) + "," + name(0x11) + "," + name(99);
        if (!"open,close,begin,unknown".equals(result)) {
            throw new RuntimeException("expected open,close,begin,unknown but got " + result);
        }
        System.out.println("CastQualifiedConstantCaseLabelVerifyTest passed!");
    }
}
