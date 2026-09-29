import java.util.concurrent.atomic.AtomicInteger;

/**
 * Bug: `while (true) { ... }` (no `break`, every exit an internal
 * `return`) as a method's own LAST statement emitted a real "push
 * condition; ifeq loop_end" test for the compile-time-constant `true`
 * condition, unlike `for (;;)`'s own already-correct handling of an
 * EMPTY condition (which skips the test entirely). Since nothing
 * follows the loop in the bytecode either (no break, no fall-through),
 * that `ifeq`'s target coincided EXACTLY with the method's own final
 * code length - a branch to an instruction that doesn't exist, which is
 * malformed regardless of any stack map frame recorded there.
 * VerifyError "Expecting a stack map frame at branch target N". Matches
 * gumdrop's own SocksServer.acquireRelay()'s "while (true) { ... }".
 */
public class WhileTrueNoBreakLastStmtVerifyTest {
    final int maxRelays;
    final AtomicInteger activeRelayCount = new AtomicInteger(0);

    WhileTrueNoBreakLastStmtVerifyTest(int maxRelays) {
        this.maxRelays = maxRelays;
    }

    boolean acquireRelay() {
        if (maxRelays <= 0) {
            activeRelayCount.incrementAndGet();
            return true;
        }
        while (true) {
            int current = activeRelayCount.get();
            if (current >= maxRelays) {
                return false;
            }
            if (activeRelayCount.compareAndSet(current, current + 1)) {
                return true;
            }
        }
    }

    /** while(true) with a real `break` must keep working too. */
    static int countToFive() {
        int i = 0;
        while (true) {
            i++;
            if (i >= 5) {
                break;
            }
        }
        return i;
    }

    public static void main(String[] args) {
        WhileTrueNoBreakLastStmtVerifyTest s = new WhileTrueNoBreakLastStmtVerifyTest(2);
        if (!s.acquireRelay()) throw new RuntimeException("expected true (1st)");
        if (!s.acquireRelay()) throw new RuntimeException("expected true (2nd)");
        if (s.acquireRelay()) throw new RuntimeException("expected false (3rd)");
        if (countToFive() != 5) throw new RuntimeException("expected 5");
    }
}
