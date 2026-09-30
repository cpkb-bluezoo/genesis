/*
 * A `long`-typed shift count (e.g. "1L << someLongVariable") is legal
 * Java - JLS 15.19 does not require the shift count itself to be int,
 * only its low-order bits are ever used - but ISHL/LSHL/ISHR/LSHR/
 * IUSHR/LUSHR all require that count as a single-word INT on the operand
 * stack regardless of the left operand's own width. codegen_expr.c's
 * shift handling (both the plain "<<"/">>"/">>>>" operators and their
 * compound "<<="/">>="/">>>=" assignment forms) skipped ANY conversion
 * for the shift count, reasoning that "the count is not part of the
 * promotion that decided op_type" - true for byte/short/char (already
 * single-word at runtime) but not for a genuinely 2-word `long`, which
 * was left as long_2nd where LSHL needs an int: VerifyError "Bad type on
 * operand stack ... long_2nd ... not assignable to integer".
 *
 * Confirmed against gumdrop's own DtlsReplayWindow.mayAccept(): "1L <<
 * delta" where "long delta = highestSeq - combinedSeq;".
 */
public class LongShiftCountVerifyTest {
    static boolean mayAccept(long highestSeq, long combinedSeq, long bitmask) {
        long delta = highestSeq - combinedSeq;
        if (delta >= 64) {
            return false;
        }
        return (bitmask & (1L << delta)) == 0L;
    }

    static long compoundShift(long value, long count) {
        value <<= count;
        return value;
    }

    public static void main(String[] args) {
        if (!mayAccept(10, 8, 0L)) {
            throw new RuntimeException("expected true");
        }
        if (compoundShift(1L, 4L) != 16L) {
            throw new RuntimeException("expected 16");
        }
        System.out.println("LongShiftCountVerifyTest passed!");
    }
}
