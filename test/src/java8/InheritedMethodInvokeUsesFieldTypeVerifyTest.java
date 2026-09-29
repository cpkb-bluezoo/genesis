/**
 * Bug: calling an inherited (not overridden) method through an IMPLICIT
 * `this`-field receiver (e.g. "argsBuilder.setLength(0);" inside the
 * declaring class itself, no explicit "this." needed) emitted an
 * invokevirtual referencing the method's actual declaring ANCESTOR
 * class instead of the field's own declared type - the same bug as
 * InheritedMethodInvokeUsesReceiverTypeVerifyTest, in a sibling code
 * path (codegen_expr.c's "field reference" receiver branch of
 * codegen_method_call(), not its plain-local-variable-identifier
 * branch). StringBuilder.setLength(int) is only ever declared on the
 * package-private java.lang.AbstractStringBuilder; genesis emitted
 * "invokevirtual AbstractStringBuilder.setLength", illegal from outside
 * java.lang even though setLength() itself is public. Confirmed against
 * gumdrop's own FtpProtocolHandler.resetLineState()'s
 * "argsBuilder.setLength(0);", argsBuilder being an instance field.
 */
public class InheritedMethodInvokeUsesFieldTypeVerifyTest {
    private StringBuilder buf = new StringBuilder("hello world");

    private void reset() {
        buf.setLength(5);
    }

    public static void main(String[] args) {
        InheritedMethodInvokeUsesFieldTypeVerifyTest t = new InheritedMethodInvokeUsesFieldTypeVerifyTest();
        t.reset();
        String result = t.buf.toString();
        if (!"hello".equals(result)) {
            throw new RuntimeException("expected 'hello', got '" + result + "'");
        }
    }
}
