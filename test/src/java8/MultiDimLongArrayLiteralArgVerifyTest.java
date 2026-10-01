/*
 * Bug: a multi-dimensional array literal (e.g. "new long[][] { { 0, 0 } }")
 * passed as a VOID method's argument corrupted genesis's own internal
 * stack-depth bookkeeping (mg->stack_depth), independently of the actual
 * emitted bytecode (which is correct). codegen_array_init()'s per-element
 * store applies an extra "wide type takes 2 slots" pop whenever the
 * array's ultimate LEAF element type is long/double - correct at the
 * leaf dimension (LASTORE/DASTORE genuinely pop 4 stack words, not 3), but
 * WRONG at any outer dimension of a multi-dimensional array, where each
 * element is actually a SUB-ARRAY REFERENCE stored via AASTORE (pops only
 * 3 words, matching any other reference store) - "long/double" there only
 * describes the eventual scalar type, not what this level's own store
 * opcode operates on. The resulting undercount left the tracked stack
 * depth after the whole array-literal-as-argument permanently 1 slot
 * lower than the REAL bytecode stack. AST_EXPR_STMT then computed
 * "slots_to_pop = stack_depth_after - stack_depth_before" as a uint16_t,
 * went negative, wrapped to 65535, and unconditionally emitted a spurious
 * POP2 after the (void-returning) call - VerifyError: "Operand stack
 * overflow" (or, with nothing else on the stack, "Operand stack
 * underflow" attempting to pop2 an empty stack). Confirmed against
 * gumdrop's own LossDetectorTest.testPersistentCongestionDropsWindowToMinimum(),
 * whose "detector.onAckReceived(..., new long[][] { { 0, 0 } }, ...)"
 * hits exactly this shape.
 */
public class MultiDimLongArrayLiteralArgVerifyTest {
    static void take(long[][] d) {
        if (d.length != 1 || d[0].length != 2 || d[0][0] != 0 || d[0][1] != 0) {
            throw new RuntimeException("wrong array contents");
        }
    }

    public static void main(String[] args) {
        take(new long[][] { { 0, 0 } });
        System.out.println("MultiDimLongArrayLiteralArgVerifyTest passed!");
    }
}
