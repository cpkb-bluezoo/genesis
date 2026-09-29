/**
 * Bug: a switch case label that's a constant EXPRESSION combining two
 * NAMED constants (e.g. "case SEQUENCE & TAG_MASK:", not a literal like
 * "case ('U' << 24) | 'S':") failed to fold - eval_int_constant_expr()
 * recursed into the binary expression's operands but had no case for a
 * bare AST_IDENTIFIER operand (a "static final int" field reference),
 * even though semantic analysis already resolves such identifiers'
 * constant values onto the leaf, exactly like a bare-identifier case
 * label already relies on elsewhere. The whole expression silently
 * failed to evaluate, leaving that case's match value at its
 * zero-initialized default - so every case shaped this way collided on
 * match value 0, and lookupswitch's own JVMS 4.9.1 requirement that
 * match values be strictly increasing made the classfile invalid the
 * instant there was more than one such case: ClassFormatError "Bad
 * lookupswitch instruction". Confirmed against gumdrop's own
 * Asn1Type.getTagName(), whose "case SEQUENCE & TAG_MASK:" and
 * "case SET & TAG_MASK:" are exactly this shape.
 */
public class SwitchCaseLabelNamedConstantOperandVerifyTest {
    static final int TAG_MASK = 0x1F;
    static final int CONSTRUCTED = 0x20;
    static final int SEQUENCE = CONSTRUCTED | 0x10;
    static final int SET = CONSTRUCTED | 0x11;
    static final int INTEGER = 0x02;

    static String describe(int tag) {
        switch (tag) {
            case INTEGER:
                return "INTEGER";
            case SEQUENCE & TAG_MASK:
                return "SEQUENCE";
            case SET & TAG_MASK:
                return "SET";
            default:
                return "UNKNOWN";
        }
    }

    public static void main(String[] args) {
        if (!"INTEGER".equals(describe(INTEGER))) {
            throw new RuntimeException("expected INTEGER, got " + describe(INTEGER));
        }
        if (!"SEQUENCE".equals(describe(SEQUENCE & TAG_MASK))) {
            throw new RuntimeException("expected SEQUENCE, got " + describe(SEQUENCE & TAG_MASK));
        }
        if (!"SET".equals(describe(SET & TAG_MASK))) {
            throw new RuntimeException("expected SET, got " + describe(SET & TAG_MASK));
        }
        if (!"UNKNOWN".equals(describe(99))) {
            throw new RuntimeException("expected UNKNOWN, got " + describe(99));
        }
    }
}
