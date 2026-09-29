/**
 * Bug: an explicit `this(...)` constructor-delegation call whose
 * argument list contains a ternary (or any other branching expression)
 * corrupted "this"'s own tracked type on genesis's internal stackmap -
 * "this" is pushed onto the stack (as the receiver for the eventual
 * invokespecial <init>) BEFORE the branching argument is evaluated, so
 * a stackmap frame recorded at the ternary's own merge point (mid-
 * argument-list, before the this()/super() call has actually run) must
 * still show "this" as uninitializedThis, matching JVMS 4.10.1.4's own
 * uninitialized-this rules - but genesis tracked it as the class's own
 * real (already-initialized) type from the moment it was pushed.
 * VerifyError "Inconsistent stackmap frames ... Type uninitializedThis
 * ... is not assignable to '<TheClass>'". Matches gumdrop's own
 * WebSocketFrame(int,byte[],boolean,boolean)'s delegating this(...)
 * call, whose masking-key argument is exactly such a ternary
 * ("masked ? generateMaskingKey() : null").
 */
public class TernaryArgInThisCallVerifyTest {
    final int opcode;
    final String key;

    TernaryArgInThisCallVerifyTest(boolean flag, int opcode, boolean masked) {
        this(true, opcode, masked, masked ? "generated" : null);
    }

    TernaryArgInThisCallVerifyTest(boolean b, int opcode, boolean masked, String key) {
        this.opcode = opcode;
        this.key = key;
    }

    public static void main(String[] args) {
        TernaryArgInThisCallVerifyTest r1 = new TernaryArgInThisCallVerifyTest(true, 1, true);
        if (!"generated".equals(r1.key)) {
            throw new RuntimeException("expected generated, got " + r1.key);
        }
        TernaryArgInThisCallVerifyTest r2 = new TernaryArgInThisCallVerifyTest(true, 2, false);
        if (r2.key != null) {
            throw new RuntimeException("expected null, got " + r2.key);
        }
    }
}
