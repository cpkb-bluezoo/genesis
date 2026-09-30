/*
 * A switch over an enum type, whose case labels are bare enum constant
 * names, silently got the WRONG ordinal for any case label whose name
 * ALSO happened to be visible via an unrelated wildcard static import
 * ("import static genesislib.SharedNameConsts.*;") - the enum-specific
 * resolution (JLS 14.11.1: a case label in an enum switch is ALWAYS an
 * enum constant of that exact type, never resolved via general scope
 * or static import) correctly assigned the real ordinal first, but a
 * SEPARATE, generic "duplicate case label" check loop (semantic.c) then
 * unconditionally re-resolved EVERY identifier case label via general
 * scope/static-import lookup too, regardless of whether the switch was
 * already known to be an enum switch - silently overwriting the
 * correct ordinal with the unrelated same-named constant's own value.
 * Duplicate lookupswitch match values, VerifyError/ClassFormatError
 * "Bad lookupswitch instruction". Confirmed against gumdrop's own
 * SocksProtocolHandler.receive()'s "switch (state)": its State enum's
 * own SOCKS5_AUTH_USERNAME_PASSWORD/SOCKS5_AUTH_GSSAPI constants share
 * names with two unrelated "byte" constants on SocksConstants, brought
 * in via the same file's "import static ...SocksConstants.*;".
 */
import static genesislib.SharedNameConsts.*;

public class EnumSwitchStaticImportNameCollisionTest {
    enum State { A, B, FOO, BAR, C }

    static String classify(State s) {
        switch (s) {
            case A: return "A";
            case B: return "B";
            case FOO: return "FOO";
            case BAR: return "BAR";
            case C: return "C";
        }
        return "?";
    }

    public static void main(String[] args) {
        if (!"FOO".equals(classify(State.FOO))) {
            System.out.println("FAILED: expected FOO, got " + classify(State.FOO));
            System.exit(1);
        }
        if (!"BAR".equals(classify(State.BAR))) {
            System.out.println("FAILED: expected BAR, got " + classify(State.BAR));
            System.exit(1);
        }
        if (!"C".equals(classify(State.C))) {
            System.out.println("FAILED: expected C, got " + classify(State.C));
            System.exit(1);
        }
        System.out.println("EnumSwitchStaticImportNameCollisionTest passed!");
    }
}
