/**
 * Bug: a bare "case NAMED_CONSTANT:" label whose constant's OWN
 * initializer is a qualified reference to ANOTHER class's constant
 * (e.g. "static final int EXT_ENCRYPTED_CLIENT_HELLO =
 * Other.EXTENSION_TYPE;") never resolved - the case-label handling's
 * bare-identifier branch only understood a plain int/char literal
 * initializer, so a constant declared this way silently defaulted its
 * case label to match value 0, colliding with any OTHER case that
 * legitimately uses 0. Confirmed against gumdrop's own
 * HandshakeMessages.parseClientHelloExtension(), whose
 * "EXT_ENCRYPTED_CLIENT_HELLO = EncryptedClientHello.EXTENSION_TYPE;"
 * collided with the legitimate "EXT_SERVER_NAME = 0x0000": duplicate
 * lookupswitch match value 0, ClassFormatError "Bad lookupswitch
 * instruction" (JVMS 4.9.1 requires strictly increasing match values).
 */
public class SwitchCaseLabelConstantWithQualifiedInitializerVerifyTest {
    static class Other {
        static final int EXTENSION_TYPE = 7;
    }

    static final int EXT_SERVER_NAME = 0x0000;
    static final int EXT_ENCRYPTED_CLIENT_HELLO = Other.EXTENSION_TYPE;

    static String describe(int extType) {
        switch (extType) {
            case EXT_SERVER_NAME:
                return "SERVER_NAME";
            case EXT_ENCRYPTED_CLIENT_HELLO:
                return "ENCRYPTED_CLIENT_HELLO";
            default:
                return "UNKNOWN";
        }
    }

    public static void main(String[] args) {
        if (!"SERVER_NAME".equals(describe(0))) {
            throw new RuntimeException("expected SERVER_NAME, got " + describe(0));
        }
        if (!"ENCRYPTED_CLIENT_HELLO".equals(describe(7))) {
            throw new RuntimeException("expected ENCRYPTED_CLIENT_HELLO, got " + describe(7));
        }
        if (!"UNKNOWN".equals(describe(99))) {
            throw new RuntimeException("expected UNKNOWN, got " + describe(99));
        }
    }
}
