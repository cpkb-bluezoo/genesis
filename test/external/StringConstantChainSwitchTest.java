import stringconstchainlib.Methods;

/* Regression test: "case NAME:" in a string switch where NAME is a
 * "static final String" of THIS class initialized from ANOTHER class's
 * constant (loaded from a class file) rather than from a literal. The
 * label's value could not be worked out through that chain, so the case
 * never matched. Mirrors gumdrop's HttpAuthenticationMethods
 * (BASIC_AUTH = HttpServletRequest.BASIC_AUTH; getScheme() switch). */
public class StringConstantChainSwitchTest {
    public static final String BASIC_AUTH = Methods.BASIC;
    public static final String DIGEST_AUTH = Methods.DIGEST;
    public static final String BEARER_AUTH = "BEARER";
    static final int LEVEL_COPY = Methods.LEVEL;

    static String scheme(String m) {
        switch (m) {
            case BASIC_AUTH: return "Basic";
            case DIGEST_AUTH: return "Digest";
            case BEARER_AUTH: return "Bearer";
            case Methods.BASIC + "-x": return "BasicX";
            default: return null;
        }
    }

    static String level(int n) {
        switch (n) {
            case LEVEL_COPY: return "three";
            default: return "other";
        }
    }

    public static void main(String[] args) {
        if (!"Basic".equals(scheme("BASIC")) || !"Digest".equals(scheme("DIGEST"))
                || !"Bearer".equals(scheme("BEARER")) || scheme("other") != null) {
            throw new RuntimeException("string chain: " + scheme("BASIC") + " " + scheme("DIGEST"));
        }
        if (!"BasicX".equals(scheme("BASIC-x"))) {
            throw new RuntimeException("concatenated constant");
        }
        if (!"three".equals(level(3)) || !"other".equals(level(4))) {
            throw new RuntimeException("int chain");
        }
        System.out.println("StringConstantChainSwitchTest passed!");
    }
}
