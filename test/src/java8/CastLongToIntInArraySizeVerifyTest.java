/**
 * Bug: an explicit "(int)" cast on a LONG-typed expression used as an
 * array creation's SIZE, e.g. "new byte[(int) ((totalBits + 7) / 8)]",
 * silently dropped the cast entirely - no L2I was emitted, leaving a
 * long (two stack words) where newarray's size operand needs a single
 * int. Root cause: the cast codegen determines its source type from the
 * operand expression's own resolved sem_type, but nothing else visits an
 * array dimension expression with the general expression type-checker
 * first, so sem_type was unset here - falling through to "assume the
 * source is already an int" (a reasonable default when it's genuinely
 * unknown), which then looked like a no-op cast (source == target == int)
 * instead of the real long-to-int narrowing it actually was.
 * VerifyError: "Bad type on operand stack ... long_2nd ... not
 * assignable to integer" at the newarray. Confirmed against gumdrop's
 * own Huffman.encode(), exactly this shape.
 */
public class CastLongToIntInArraySizeVerifyTest {
    public static void main(String[] args) {
        long totalBits = 100;
        byte[] out = new byte[(int) ((totalBits + 7) / 8)];
        if (out.length != 13) {
            throw new RuntimeException("expected length 13, got " + out.length);
        }

        long bigger = 12345678901L;
        int[] sized = new int[(int) (bigger % 1000)];
        if (sized.length != 901) {
            throw new RuntimeException("expected length 901, got " + sized.length);
        }
    }
}
