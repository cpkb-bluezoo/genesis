/**
 * Bug: a switch case label that is a compile-time constant EXPRESSION
 * (not a bare literal or a named constant, e.g. combining char literals
 * with shift/bitwise-or into a packed int - a common way to switch on a
 * multi-character command/tag) fell through codegen_stmt.c's case-value
 * collection entirely, since it only ever handled AST_LITERAL and
 * AST_IDENTIFIER case labels. Every such case's match value silently
 * stayed at its zero-initialized default, so multiple case labels
 * collided on match value 0 and the JVM verifier rejected the resulting
 * lookupswitch outright: VerifyError "Bad lookupswitch instruction".
 * Confirmed against gumdrop's own FtpProtocolHandler.matchCommand(),
 * whose ~40 case labels are all shaped exactly like this (e.g.
 * "case ('U' << 24) | ('S' << 16) | ('E' << 8) | 'R':" for the FTP
 * "USER" command), packing 4 command-name bytes into one int per case.
 */
public class SwitchCaseLabelConstantExpressionVerifyTest {
    static int pack4(char a, char b, char c, char d) {
        return (a << 24) | (b << 16) | (c << 8) | d;
    }

    static String classify(int packed) {
        switch (packed) {
            case ('U' << 24) | ('S' << 16) | ('E' << 8) | 'R':
                return "USER";
            case ('P' << 24) | ('A' << 16) | ('S' << 8) | 'S':
                return "PASS";
            case ('Q' << 24) | ('U' << 16) | ('I' << 8) | 'T':
                return "QUIT";
            case ('L' << 24) | ('I' << 16) | ('S' << 8) | 'T':
                return "LIST";
            default:
                return "UNKNOWN";
        }
    }

    public static void main(String[] args) {
        check("USER", classify(pack4('U', 'S', 'E', 'R')));
        check("PASS", classify(pack4('P', 'A', 'S', 'S')));
        check("QUIT", classify(pack4('Q', 'U', 'I', 'T')));
        check("LIST", classify(pack4('L', 'I', 'S', 'T')));
        check("UNKNOWN", classify(pack4('Z', 'Z', 'Z', 'Z')));
    }

    private static void check(String expected, String actual) {
        if (!expected.equals(actual)) {
            throw new RuntimeException("expected " + expected + ", got " + actual);
        }
    }
}
