/*
 * Regression test: an array literal whose element type is wider than
 * int (e.g. long[]) with one element being a ternary expression (e.g.
 * "new long[] { sequenceNumber, multiple ? 1 : 0 }", matching gumdrop's
 * own AMQP ConfirmListener test:
 * "acks.add(new long[] { sequenceNumber, multiple ? 1 : 0 });") used to
 * compile silently but generate a broken StackMapTable entry:
 *
 *   java.lang.VerifyError: Inconsistent stackmap frames at branch target N
 *   Reason: Type '[J' (current frame, stack[...]) is not assignable to
 *           integer (stack map, stack[...])
 *
 * Root cause: codegen_array_init() (codegen_expr.c) tracks the array's
 * own size push via mg_push_int() before emitting newarray/anewarray,
 * then emits newarray/anewarray itself - which replaces that size int
 * with the new array reference on the REAL operand stack (a genuine
 * type change, even though the word count is unchanged for a
 * single-dimension array) - but never corrected the corresponding
 * tracked type on mg->stackmap to match: the array's own creation left
 * a stale "Integer" verification-type entry sitting where the array
 * reference now actually is. For a straight-line element store (no
 * branch), this stale entry is invisible - no StackMapTable frame gets
 * recorded mid-store to observe it. But once any later element's own
 * value requires recording a frame at a branch target (e.g. a ternary,
 * which needs one at its false-branch entry), that recording call
 * serializes the stale, buried "Integer" entry as part of the frame,
 * corrupting it. Fixed by correcting the tracked type right after
 * building the array (mg_pop_typed(1) + mg_push_object(array_type_desc))
 * to match what newarray/anewarray/multianewarray actually leaves on
 * the real stack.
 */
public class TernaryElementInWideArrayLiteralVerifyTest {
    static long[] make(long sequenceNumber, boolean multiple) {
        return new long[] { sequenceNumber, multiple ? 1 : 0 };
    }

    public static void main(String[] args) {
        long[] a = make(2L, true);
        long[] b = make(5L, false);
        if (a[0] != 2L || a[1] != 1L) {
            throw new RuntimeException("expected {2,1} but got {" + a[0] + "," + a[1] + "}");
        }
        if (b[0] != 5L || b[1] != 0L) {
            throw new RuntimeException("expected {5,0} but got {" + b[0] + "," + b[1] + "}");
        }
        System.out.println("TernaryElementInWideArrayLiteralVerifyTest passed!");
    }
}
