package crossfilestaticfieldassign;

/**
 * Bug: assigning to a static field of a DIFFERENT class from another
 * file (e.g. "Holder.value = v;" inside User.java) generated an
 * "instance field assignment" (DUP_X1/PUTFIELD) instead of a static one
 * (DUP/PUTSTATIC), because codegen_expr.c's codegen_assignment() only
 * ever recognized the receiver as a class name via resolve_class_name()
 * - a small hardcoded whitelist of well-known JDK classes plus the
 * current class and its own nested classes, explicitly marked
 * "TODO: Check imports". Falling through to the instance-field path
 * evaluated the bare class-name AST_IDENTIFIER as an expression (a
 * no-op, since it isn't a real value), so only the assigned VALUE ended
 * up on the stack where [receiver, value] was expected - the
 * DUP_X1/PUTFIELD sequence that followed saw a short stack:
 * VerifyError: "Operand stack underflow" / "Attempt to pop empty
 * stack". Confirmed against gumdrop's own ZoneFilePersistenceTest
 * ("StorageExecutor.workThreadObserver = ...", imported from a
 * different package).
 */
public class CrossFileStaticFieldAssignVerifyTest {
    public static void main(String[] args) {
        Holder.value = null;
        Holder.counter = 0;

        User.setValue("hello");
        if (!"hello".equals(Holder.value)) {
            throw new RuntimeException("expected Holder.value=hello, got " + Holder.value);
        }

        User.bumpCounter();
        if (Holder.counter != 5L) {
            throw new RuntimeException("expected Holder.counter=5, got " + Holder.counter);
        }
    }
}
