/**
 * Bug: a primitive-to-primitive cast between two types with the SAME
 * JVM word count on either side (double<->long, both 2 words;
 * int<->float, both 1 word) emitted the correct conversion opcode
 * (D2L, L2D, I2F, F2I) but never updated genesis's own internal
 * stackmap type tracking - it left the SOURCE type's stale tag in
 * place, since the raw word count didn't need adjusting. A cast
 * WIDENING/NARROWING the word count (e.g. int->long, long->int) also
 * had a related gap: it corrected the raw word count but, for the wide
 * side, only touched ONE of that type's two required stackmap entries.
 * Both stayed invisible for straight-line code, but corrupted any
 * StackMapTable frame recorded while the cast result is still on the
 * stack - e.g. a ternary branch evaluating "(long) (doubleExpr)":
 * VerifyError "Inconsistent stackmap frames ... Type long ... is not
 * assignable to double". Matches gumdrop's own
 * MdnsCache.scheduleNextRefreshStage()'s
 * "stage == 0 ? (long) (ttlMs * FRACTIONS[0]) : (long) (...)".
 */
public class TernaryPrimitiveCastStackmapVerifyTest {
    static double[] FRACTIONS = { 0.5, 0.75, 0.9 };

    static long delayFor(int stage, long ttlMs) {
        long delay = stage == 0
                ? (long) (ttlMs * FRACTIONS[0])
                : (long) (ttlMs * (FRACTIONS[stage] - FRACTIONS[stage - 1]));
        return delay;
    }

    static float pickAsFloat(boolean flag, int a, int b) {
        return flag ? (float) a : (float) b;
    }

    public static void main(String[] args) {
        long r1 = delayFor(0, 1000L);
        if (r1 != 500L) {
            throw new RuntimeException("expected 500, got " + r1);
        }
        long r2 = delayFor(1, 1000L);
        if (r2 != 250L) {
            throw new RuntimeException("expected 250, got " + r2);
        }
        if (pickAsFloat(true, 3, 7) != 3.0f) {
            throw new RuntimeException("expected 3.0");
        }
        if (pickAsFloat(false, 3, 7) != 7.0f) {
            throw new RuntimeException("expected 7.0");
        }
    }
}
