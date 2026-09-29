/*
 * Regression test: an if/else statement inside a loop whose THEN branch
 * ends with an unconditional `break;` - matching gumdrop's own
 * DnsMessage.decodeName(), whose compression-pointer branch of an
 * if/else appends to a StringBuilder and then unconditionally breaks out
 * of the enclosing while loop. Used to throw at class-verification time:
 *
 *   java.lang.VerifyError: Expecting a stack map frame
 *   Reason: Expected stackmap frame at this location.
 *
 * Root cause: AST_IF_STMT's own "does this branch terminate" checks
 * (then_ends_with_return / else_ends_with_return in codegen_stmt.c)
 * recognized only return/throw opcodes, never OP_GOTO - unlike the
 * equivalent, already-fixed check for a catch clause's own ends-with-
 * return test elsewhere in the same file, which explicitly treats
 * OP_GOTO (emitted by break/continue) the same as return/throw. A
 * `break` ending the then branch (jumping straight to the loop's own
 * exit) genuinely never falls through to after the if-statement either
 * - but since it wasn't recognized as terminating, the if-statement's
 * own join-point "goto past else" still got emitted anyway, landing
 * directly after the break's own goto: dead, unreachable code with no
 * stack map frame.
 *
 * Fixed by adding OP_GOTO to both then_ends_with_return and
 * else_ends_with_return.
 */
public class IfBranchEndingInBreakVerifyTest {
    static String decode(int[] data) {
        StringBuilder name = new StringBuilder();
        int i = 0;
        while (i < data.length) {
            int len = data[i++];
            if (len == 0) {
                break;
            }
            if (len < 0) {
                if (name.length() > 0) {
                    name.append('.');
                }
                name.append("PTR");
                break;
            } else {
                if (name.length() > 0) {
                    name.append('.');
                }
                name.append("L").append(len);
            }
        }
        return name.toString();
    }

    public static void main(String[] args) {
        String a = decode(new int[]{3, 5, -1, 0});
        String b = decode(new int[]{3, 5, 0});

        if (!"L3.L5.PTR".equals(a)) {
            throw new RuntimeException("expected L3.L5.PTR, got " + a);
        }
        if (!"L3.L5".equals(b)) {
            throw new RuntimeException("expected L3.L5, got " + b);
        }

        System.out.println("IfBranchEndingInBreakVerifyTest passed!");
    }
}
