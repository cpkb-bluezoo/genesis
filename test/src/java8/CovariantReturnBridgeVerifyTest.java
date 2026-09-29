/*
 * Regression test: a subclass overriding a generic superclass method with a
 * COVARIANT RETURN TYPE (same parameter list, only the return type is more
 * specific than the superclass's own erased one) was silently never invoked
 * through the superclass's own entry points - no VerifyError, no
 * AbstractMethodError, just silently wrong behavior at runtime.
 *
 * Concretely (found via gumdrop's DirectByteBufferPool, which subclasses
 * java.lang.ThreadLocal<ByteBuffer[]> exactly this way):
 *
 *   new ThreadLocal<int[]>() {
 *       protected int[] initialValue() { return new int[]{42}; }
 *   }
 *
 * java.lang.ThreadLocal<T> declares "protected T initialValue()", which
 * erases to "protected Object initialValue()" - the only signature that
 * exists in ThreadLocal's own classfile. The anonymous subclass above
 * declares "protected int[] initialValue()" - a real override by JLS rules,
 * but with a DIFFERENT (more specific) erased descriptor than the method it
 * overrides. For the JVM's own virtual dispatch to work when
 * ThreadLocal.get()'s internal code calls "this.initialValue()" (compiled
 * against the erased "()Ljava/lang/Object;" descriptor, since that's the
 * only one ThreadLocal itself declares), the subclass needs a SYNTHETIC
 * BRIDGE method with that exact erased descriptor:
 *
 *   public synthetic bridge Object initialValue() {
 *       return this.initialValue();   // invokevirtual -> the real, covariant override
 *   }
 *
 * Without it, "Object initialValue()" doesn't exist anywhere in the
 * subclass, so the call falls back through the class hierarchy to
 * ThreadLocal's OWN base implementation ("return null;") - silently, not
 * with an AbstractMethodError, since ThreadLocal.initialValue() is concrete.
 * The real override is never invoked at all, and ThreadLocal.get() always
 * returns null instead of the real initial value.
 *
 * Root cause: codegen.c had exactly two bridge-generation functions -
 * generate_superclass_bridges() (forwards an INHERITED-but-NOT-overridden
 * generic method straight to super, skipping entirely whenever the subclass
 * already has its own method with the same name/arity - the wrong case for
 * us) and generate_interface_bridges() (handles the analogous "overridden
 * with a covariant/concrete signature" case, but only for interfaces, not
 * for a superclass). Neither generated the bridge this needs. Fixed by
 * adding a new generate_covariant_override_bridges() - the mutually
 * exclusive complement to generate_superclass_bridges(): it fires exactly
 * when the subclass DOES override an inherited generic superclass method,
 * but with an erased descriptor that differs from the superclass method's
 * own erasure, emitting a synthetic bridge at the superclass's own erased
 * descriptor that invokevirtual-calls the real, concrete override directly
 * (not invokespecial/super, since there's a real override to reach).
 *
 * Before the fix: prints "FAIL: initialValue() never ran, got null".
 * After the fix: prints "CovariantReturnBridgeVerifyTest passed!".
 */
public class CovariantReturnBridgeVerifyTest {

    private static final ThreadLocal<int[]> LOCAL = new ThreadLocal<int[]>() {
        @Override
        protected int[] initialValue() {
            return new int[] { 42 };
        }
    };

    public static void main(String[] args) {
        int[] arr = LOCAL.get();
        if (arr == null) {
            throw new RuntimeException(
                "FAIL: initialValue() never ran, got null");
        }
        if (arr.length != 1 || arr[0] != 42) {
            throw new RuntimeException(
                "FAIL: unexpected value from initialValue()");
        }
        System.out.println("CovariantReturnBridgeVerifyTest passed!");
    }
}
