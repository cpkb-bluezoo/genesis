/**
 * Bug: calling an inherited method that a class doesn't override itself
 * (so the method is only ever declared on some ANCESTOR class) emitted
 * an invokevirtual/invokeinterface referencing the ANCESTOR class in the
 * constant pool, instead of the receiver expression's own static type -
 * unlike javac, which always binds the symbolic reference to the
 * receiver's compile-time type and lets the JVM's normal virtual
 * dispatch walk the hierarchy at runtime.
 *
 * This is invisible whenever the declaring ancestor is just as
 * accessible as the receiver's own type (the overwhelmingly common
 * case), but breaks the moment the declaring ancestor is LESS
 * accessible - e.g. StringBuilder.setLength(int) is only ever declared
 * on the package-private java.lang.AbstractStringBuilder (StringBuilder
 * doesn't need to override it - no covariant return, unlike append()).
 * Calling code outside java.lang got "invokevirtual
 * AbstractStringBuilder.setLength", illegal even though setLength()
 * itself is public: java.lang.IllegalAccessError: "failed to access
 * class java.lang.AbstractStringBuilder". Confirmed against gumdrop's
 * own SaslUtils.parseDigestParams() ("key.setLength(0);
 * value.setLength(0);").
 */
public class InheritedMethodInvokeUsesReceiverTypeVerifyTest {
    public static void main(String[] args) {
        StringBuilder sb = new StringBuilder("hello world");
        sb.setLength(5);
        String result = sb.toString();
        if (!"hello".equals(result)) {
            throw new RuntimeException("expected 'hello', got '" + result + "'");
        }
    }
}
