package varargsarrayelement;

/* Regression test: a varargs parameter whose ELEMENT type is itself an
 * array ("byte[]... args"), called from a different file than the one
 * declaring it, compiled together in one batch.
 *
 * The cross-file view of such a parameter skipped the extra array
 * dimension ("already an array") and typed it byte[] instead of byte[][].
 * Callers then packed byte[] arguments into a byte[] with BASTORE
 * (VerifyError), and with a String... overload alongside, the byte[]...
 * one was never chosen correctly (NoSuchMethodError, VerifyError, or
 * "cannot convert <unknown>"). Mirrors gumdrop's RedisSession.command
 * overload pair.
 *
 * See test/external/ExternalVarargsArrayElementTest.java for the same
 * calls made against class files. */
public class VarargsArrayElementVerifyTest {

    private static void check(String expected, String actual, String what) {
        if (!expected.equals(actual)) {
            throw new RuntimeException(what + ": expected " + expected + ", got " + actual);
        }
    }

    public static void main(String[] args) {
        Impl impl = new Impl();
        Session session = impl;
        byte[] one = { 1 };
        byte[] two = { 2, 3 };
        byte[][] both = { one, two };
        String[] words = { "a", "b", "c" };

        /* Through the interface */
        check("B:SET:1/1", session.command("h", "SET", one), "interface, one byte[]");
        check("B:SET:2/3", session.command("h", "SET", one, two), "interface, two byte[]");
        check("B:SET:1/2", session.command("h", "SET", new byte[] { 4, 5 }), "interface, new byte[]");
        check("B:SET:2/3", session.command("h", "SET", both), "interface, byte[][] passed through");
        check("S:SET:3", session.command("h", "SET", words), "interface, String[] passed through");
        check("S:GET:2", session.command("h", "GET", "k", "v"), "interface, Strings");
        check("S:GET:1", session.command("h", "GET", "k"), "interface, one String");

        /* Through the class */
        check("B:SET:1/1", impl.command("h", "SET", one), "class, one byte[]");
        check("B:SET:2/3", impl.command("h", "SET", one, two), "class, two byte[]");
        check("B:SET:2/3", impl.command("h", "SET", both), "class, byte[][] passed through");
        check("S:GET:2", impl.command("h", "GET", "k", "v"), "class, Strings");

        /* No overload involved, element type still an array */
        check("2", String.valueOf(session.pieces(one, two)), "pieces, two byte[]");
        check("2", String.valueOf(session.pieces(both)), "pieces, byte[][] passed through");
        check("0", String.valueOf(session.pieces()), "pieces, no arguments");

        /* Static, with a primitive array element type */
        check("2,1", Impl.join(',', new int[] { 1, 2 }, new int[] { 3 }), "static int[]...");

        System.out.println("PASS");
    }
}
