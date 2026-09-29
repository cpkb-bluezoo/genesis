/*
 * Regression test: an implicit `this.field = value;` assignment whose
 * right-hand side is a ternary conditioned on a null comparison (`x !=
 * null ? ... : ...`) - exactly gumdrop's own TcpEndpoint.init() shape:
 *
 *   bufferSize = (socket != null)
 *           ? Math.max(DEFAULT_BUFFER_SIZE, socket.getReceiveBufferSize())
 *           : DEFAULT_BUFFER_SIZE;
 *
 * used to throw:
 *
 *   java.lang.VerifyError: Inconsistent stackmap frames
 *   Type 'ClassName' (current frame, stack[0]) is not assignable to
 *   null (stack map, stack[0])
 *
 * Root cause: codegen_assignment()'s "Instance field assignment:
 * this.field = value" codegen (codegen_expr.c) emits ALOAD_0 to push the
 * real `this` reference (needed later for the field's own putfield), but
 * tracked it on genesis's own stackmap bookkeeping via mg_push_null() -
 * recording a VT_NULL entry for what is actually a known, non-null
 * object reference. Harmless as long as `this` is consumed immediately,
 * but the null-comparison inside the RHS's own ternary condition
 * materializes its own boolean via the classic
 * `if_acmpeq/ifne target; iconst_1; goto end; target: iconst_0; end:`
 * pattern, recording a stack-map frame at its own internal branch target
 * while `this` is STILL buried on the stack underneath the comparison's
 * own two operands - and that recorded frame inherited the wrong
 * (null) type for that slot, rejected the moment the real `this`
 * reference reached it. The identical mistake (aload_0 immediately
 * followed by mg_push_null() instead of mg_push_object()) was found and
 * fixed in three sibling `this`-pushing sites in the same file too (a
 * captured-outer-variable relay load, an inherited-field assignment, and
 * an invokedynamic lambda's captured-`this` argument).
 */
public class InstanceFieldAssignNullComparisonVerifyTest {
    int bufferSize;
    static final int DEFAULT_BUFFER_SIZE = 4096;

    void init(String socket) {
        bufferSize = (socket != null)
                ? Math.max(DEFAULT_BUFFER_SIZE, socket.length())
                : DEFAULT_BUFFER_SIZE;
    }

    public static void main(String[] args) {
        InstanceFieldAssignNullComparisonVerifyTest t = new InstanceFieldAssignNullComparisonVerifyTest();
        t.init(null);
        if (t.bufferSize != DEFAULT_BUFFER_SIZE) {
            throw new AssertionError("expected default, got " + t.bufferSize);
        }
        t.init("this is a longer string than 4096 chars".repeat(200));
        if (t.bufferSize <= DEFAULT_BUFFER_SIZE) {
            throw new AssertionError("expected larger than default, got " + t.bufferSize);
        }
        System.out.println("InstanceFieldAssignNullComparisonVerifyTest passed!");
    }
}
