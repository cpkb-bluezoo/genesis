import protectedfieldchainmid.Mid;

/*
 * Regression test: a protected field of a superclass that is only available
 * as a javac-compiled .class file is visible, by its simple name, in a
 * subclass whose own superclass is a source class extending that base.
 */
public class ProtectedFieldViaSourceSubclassTest extends Mid {
    String read() {
        return label;
    }

    boolean isOk() {
        return label.equals("ok");
    }

    public static void main(String[] args) {
        ProtectedFieldViaSourceSubclassTest t = new ProtectedFieldViaSourceSubclassTest();
        if (!"ok".equals(t.read()) || !t.isOk()) {
            throw new AssertionError();
        }
        System.out.println("ProtectedFieldViaSourceSubclassTest passed!");
    }
}
