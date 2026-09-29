/**
 * Bug: a switch statement's merge point (right after the switch, where
 * every `break` and the physically last case's own fallthrough all
 * converge) was framed from whatever local-variable state the LAST case
 * processed happened to leave behind - not a proper merge of every path
 * that actually reaches it. When only ONE case (typically `default`,
 * physically last) declares its own local variable (with no braces
 * around that case's statements, so nothing else scopes it), every
 * OTHER case's `break` reaches that same merge point WITHOUT that local
 * ever being assigned - the recorded frame's claim that the slot holds
 * a real type disagreed with the actual state on every other incoming
 * edge: VerifyError "Inconsistent stackmap frames ... not assignable".
 * Confirmed against gumdrop's own FtpProtocolHandler.dispatchCommand(),
 * whose `default:` is the only one of ~44 cases to declare a local
 * ("String message").
 */
public class SwitchLocalOnlyInOneCaseVerifyTest {
    enum Kind { A, B, C }

    static int seen;

    static void dispatch(Kind kind) {
        switch (kind) {
            case A:
                seen = 1;
                break;
            case B:
                seen = 2;
                break;
            default:
                String message = "fallback:" + kind;
                seen = message.length();
        }
    }

    public static void main(String[] args) {
        dispatch(Kind.A);
        if (seen != 1) {
            throw new RuntimeException("expected seen=1 for A, got " + seen);
        }
        dispatch(Kind.B);
        if (seen != 2) {
            throw new RuntimeException("expected seen=2 for B, got " + seen);
        }
        dispatch(Kind.C);
        if (seen != "fallback:C".length()) {
            throw new RuntimeException("expected seen=" + "fallback:C".length() + " for C, got " + seen);
        }
    }
}
