/*
 * "useX ? a.length : b.length" left mg->stackmap's tracked TYPE for the
 * top-of-stack slot as the array's own reference type ('[B') instead of
 * integer after OP_ARRAYLENGTH - codegen_field_access()'s array-length
 * path (codegen_expr.c) emitted the instruction with no mg_pop_typed()/
 * mg_push_int() follow-up at all, reasoning (correctly, for word count)
 * that "arrayref -> int" has net stack-depth effect zero, but leaving the
 * simulated TYPE stale. Invisible in straight-line code - nothing ever
 * reads the stale type back out - but a real bug the moment a stackmap
 * frame is recorded while the length is still on the stack, e.g. as one
 * arm of a ternary merged with the other arm's own (correctly-typed)
 * integer: VerifyError "Type integer ... is not assignable to '[B'".
 *
 * Confirmed against gumdrop's own Encoder.encode() (org.bluezoo.gumdrop.
 * http.hpack): "useHuffman ? hname.length : rname.length".
 */
public class ArrayLengthTernaryVerifyTest {
    static int pick(boolean useSecond, byte[] a, byte[] b) {
        int len = useSecond ? b.length : a.length;
        return len;
    }

    public static void main(String[] args) {
        int r = pick(true, new byte[] { 1, 2 }, new byte[] { 1, 2, 3 });
        if (r != 3) {
            throw new RuntimeException("expected 3, got " + r);
        }
        System.out.println("ArrayLengthTernaryVerifyTest passed!");
    }
}
