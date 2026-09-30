/**
 * Bug: an enhanced for-loop whose own declared loop-variable type is
 * ITSELF an array (e.g. "for (long[] range : ranges)", iterating a
 * "long[][]") never got a CHECKCAST after the AALOAD that fetches each
 * element - only a loop variable declared as a plain class type
 * (AST_CLASS_TYPE) got one. mg->stackmap's own tracked type for the
 * loop variable was ALSO left as a generic "java/lang/Object" for the
 * same reason (only AST_CLASS_TYPE was handled there too). Both defects
 * stayed invisible for straight-line code (the verifier's own forward
 * dataflow narrows the real type regardless), but an explicit stack
 * map frame recorded mid-loop-body (e.g. from an `if`/`continue`)
 * exposed the stale, wrong tracked type: VerifyError "Bad type on
 * operand stack ... is not assignable to '[J'" (or "Inconsistent
 * stackmap frames", depending on where the frame lands). Matches
 * gumdrop's own LossDetector.detectAndRemoveAckedPackets()'s
 * "for (long[] range : ackRanges) { if (range[0] > range[1]) { continue; } ... }".
 */
public class NestedArrayForEachVerifyTest {
    static long sumRanges(long[][] ranges) {
        long total = 0;
        for (long[] range : ranges) {
            if (range[0] > range[1]) {
                continue;
            }
            total += range[1] - range[0];
        }
        return total;
    }

    public static void main(String[] args) {
        long[][] ranges = { { 1L, 5L }, { 20L, 10L }, { 10L, 20L } };
        long result = sumRanges(ranges);
        if (result != 14L) {
            throw new RuntimeException("expected 14, got " + result);
        }
    }
}
