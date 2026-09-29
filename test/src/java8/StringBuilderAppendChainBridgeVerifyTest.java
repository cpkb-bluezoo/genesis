/**
 * Bug: a chained call onto a JDK library method whose class overrides an
 * interface method with a covariant return type - e.g.
 * StringBuilder.append(char), overriding Appendable.append(char) but
 * returning StringBuilder instead of Appendable - picked the wrong
 * overload when resolving the call from a classfile.
 *
 * A covariant-return override gets a synthetic BRIDGE method in the
 * classfile with the same parameter types as the real method but the
 * overridden (interface/superclass) method's erased return type:
 * StringBuilder.class actually contains both
 * "append(char)Ljava/lang/StringBuilder;" (real) and
 * "append(char)Ljava/lang/Appendable;" (bridge, ACC_BRIDGE|ACC_SYNTHETIC).
 * semantic.c's classfile method loader (the "Load methods" loop building
 * symbols for a class read from a .class file) loaded BOTH into the
 * symbol table under different keys (return type is part of the raw
 * descriptor, even though Java overload resolution never uses return
 * type to choose between candidates) - so a call like "sb.append(' ')"
 * could resolve to the bridge just as easily as the real method, making
 * the expression's static type Appendable instead of StringBuilder.
 * Every subsequent call in the same chain then resolved against
 * Appendable's much smaller method set; a call Appendable has no match
 * for (e.g. append(int)) silently fell back to "assume current class",
 * producing an invokevirtual whose declaring class was the ENCLOSING
 * class - VerifyError: "Bad type on operand stack" (confirmed against
 * gumdrop's own ZoneFileWriter.formatRecord()'s MX-record case, which
 * chains append(char)/append(int)/append(char)/append(String)).
 */
public class StringBuilderAppendChainBridgeVerifyTest {
    public static void main(String[] args) {
        StringBuilder sb = new StringBuilder();
        sb.append(' ').append(5).append(' ').append("x");
        String result = sb.toString();
        if (!" 5 x".equals(result)) {
            throw new RuntimeException("expected ' 5 x', got '" + result + "'");
        }
    }
}
