/*
 * Regression test: pre/post increment or decrement of a `long[]`/`double[]`
 * array element (e.g. "counts[bucket]++") - matching gumdrop's own
 * DoubleHistogram.HistogramBuckets.record(), whose "counts[bucket]++" on
 * a long[] hits exactly this codegen path. Used to throw at class-
 * verification time:
 *
 *   java.lang.VerifyError: Operand stack overflow
 *   Reason: Exceeded max stack size.
 *
 * Root cause: codegen_expr.c's array-element ++/-- codegen (both post-
 * and pre-increment) loads the current element value via LALOAD/DALOAD
 * (after DUP2-ing the array reference and index), then unconditionally
 * treated that load as reducing the tracked stack depth by exactly 1
 * word - correct for a narrow (1-word) element, where LOAD pops arr+i
 * (2 words) and pushes a 1-word value (net -1), but wrong for a WIDE
 * (long/double, 2-word) element, where the same load pops arr+i (2
 * words) and pushes a 2-word value (net 0, not -1). This undercounted
 * the tracked stack depth by one word right after the load, which then
 * propagated through every later push/pop in the same sequence,
 * ultimately computing the method's own max_stack one word too small.
 *
 * Fixed by making that one pop conditional: 0 for a wide element, 1 for
 * a narrow one (every other push/pop amount in the same sequence was
 * already correct).
 */
public class WideArrayElementIncDecStackDepthVerifyTest {
    static long[] longs = new long[4];
    static double[] doubles = new double[4];

    public static void main(String[] args) {
        longs[1]++;
        longs[1]++;
        if (longs[1] != 2) {
            throw new RuntimeException("expected longs[1] == 2, got " + longs[1]);
        }

        long preResult = ++longs[2];
        if (preResult != 1 || longs[2] != 1) {
            throw new RuntimeException("expected pre-increment 1, got " + preResult + "/" + longs[2]);
        }

        long postResult = longs[2]++;
        if (postResult != 1 || longs[2] != 2) {
            throw new RuntimeException("expected post-increment old value 1, got " + postResult + "/" + longs[2]);
        }

        doubles[3]++;
        doubles[3]++;
        if (doubles[3] != 2.0) {
            throw new RuntimeException("expected doubles[3] == 2.0, got " + doubles[3]);
        }

        double preD = ++doubles[0];
        if (preD != 1.0 || doubles[0] != 1.0) {
            throw new RuntimeException("expected pre-increment 1.0, got " + preD + "/" + doubles[0]);
        }

        double postD = doubles[0]--;
        if (postD != 1.0 || doubles[0] != 0.0) {
            throw new RuntimeException("expected post-decrement old value 1.0, got " + postD + "/" + doubles[0]);
        }

        System.out.println("WideArrayElementIncDecStackDepthVerifyTest passed!");
    }
}
