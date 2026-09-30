/* Regression test: choosing between two varargs overloads that differ
 * only in the varargs element type, one of which is itself an array
 * (String... vs byte[]...), where the declaring type is loaded from a
 * CLASS FILE. Calls meant for the byte[]... overload were rejected
 * ("cannot convert <unknown> to java.lang.String") through an interface
 * receiver, and a byte[][] passed straight through was wrapped a second
 * time (ArrayStoreException at runtime).
 *
 * Uses the VarargsSession/VarargsImpl library shared with
 * ExternalVarargsFlagTest. See
 * test/src/java8/varargsarrayelement/ for the same calls compiled
 * together with their declarations. */
public class ExternalVarargsArrayElementTest {

    private static void check(String expected, String actual, String what) {
        if (!expected.equals(actual)) {
            throw new RuntimeException(what + ": expected " + expected + ", got " + actual);
        }
    }

    public static void main(String[] args) {
        VarargsImpl impl = VarargsImpl.open();
        VarargsSession session = impl;
        byte[] one = { 1 };
        byte[] two = { 2, 3 };
        byte[][] both = { one, two };
        String[] words = { "a", "b", "c" };

        /* Through the interface */
        check("B:SET:1", session.command("h", "SET", one), "interface, one byte[]");
        check("B:SET:2", session.command("h", "SET", one, two), "interface, two byte[]");
        check("B:SET:1", session.command("h", "SET", new byte[] { 4, 5 }), "interface, new byte[]");
        check("B:SET:2", session.command("h", "SET", both), "interface, byte[][] passed through");
        check("S:SET:3", session.command("h", "SET", words), "interface, String[] passed through");
        check("S:GET:2", session.command("h", "GET", "k", "v"), "interface, Strings");
        check("S:GET:1", session.command("h", "GET", "k"), "interface, one String");

        /* Through the class */
        check("B:SET:1", impl.command("h", "SET", one), "class, one byte[]");
        check("B:SET:2", impl.command("h", "SET", one, two), "class, two byte[]");
        check("B:SET:2", impl.command("h", "SET", both), "class, byte[][] passed through");
        check("S:GET:2", impl.command("h", "GET", "k", "v"), "class, Strings");


        System.out.println("PASS");
    }
}
