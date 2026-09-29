/*
 * Regression test: a varargs parameter whose declared element type is
 * itself an array type (`byte[]... parts`, really `byte[][]`), called
 * with more than one explicit argument - exactly gumdrop's own
 * Hpke.concat(byte[]... parts) shape - used to throw:
 *
 *   java.lang.VerifyError: Bad type on operand stack
 *   Type '[B' (current frame, stack[N]) is not assignable to integer
 *
 * at a `bastore` inside the CALLER building the synthetic varargs
 * array, and (once that was fixed in isolation) a second, different
 * error inside the declaring METHOD's own body:
 *
 *   java.lang.VerifyError: Instruction type does not match stack map
 *   Type '[[B' (current frame, locals[0]) is not assignable to '[B'
 *
 * Root cause: THREE independent places all made the exact same
 * "already an array, so varargs adds no extra dimension" mistake for a
 * varargs parameter whose written element type is itself an array -
 * correct only for the common "T... parts" case (T not itself an
 * array), but wrong whenever T is an array (varargs always adds
 * exactly one MORE dimension on top of whatever was written, the same
 * as for a scalar/class element type):
 *
 * 1. codegen_expr.c's varargs call-site codegen (three near-identical
 *    call sites) read a varargs parameter's per-argument element type
 *    via `varargs_param->type->data.array_type.element_type` directly -
 *    correct only when the parameter's own type has exactly one
 *    dimension, but this strips ALL the way to the base scalar type
 *    regardless of how many dimensions the parameter itself has. Fixed
 *    with a shared `varargs_element_type()` helper that properly
 *    accounts for `dimensions > 1`. The array-creation/stackmap-
 *    tracking logic building the synthetic varargs array itself also
 *    had no case at all for an array-typed element (defaulting to a
 *    generic, wrong "Object[]"/BASTORE) - fixed by adding one, mirroring
 *    the pattern already used for a class-typed element.
 * 2. semantic.c's main parameter-registration pass (used for a plain,
 *    ordinary method/constructor declaration) only wrapped a varargs
 *    parameter's resolved type in one more array dimension when it
 *    *wasn't* already `TYPE_ARRAY` - skipping the wrap entirely when
 *    the written type was itself an array, leaving `byte[]... parts`
 *    typed as plain `byte[]` instead of the real `byte[][]`. Fixed by
 *    always wrapping when varargs (safe/idempotent here since the type
 *    is freshly re-derived from the type node's own cached resolution
 *    each time, not from the parameter's own previous value).
 * 3. codegen.c's own parameter-slot registration (for the declaring
 *    method's own LocalVariableTable/StackMapTable entry) only
 *    prepended the extra varargs array dimension to the descriptor when
 *    it *didn't already start with '['* - the identical mistake as (2),
 *    just against a descriptor string instead of a type_t. Fixed by
 *    prepending unconditionally.
 */
public class VarargsArrayElementTypeVerifyTest {
    private static byte[] concat(byte[]... parts) {
        int total = 0;
        for (byte[] p : parts) {
            total += p.length;
        }
        byte[] result = new byte[total];
        int offset = 0;
        for (byte[] p : parts) {
            System.arraycopy(p, 0, result, offset, p.length);
            offset += p.length;
        }
        return result;
    }

    public static void main(String[] args) {
        byte[] a = { 1, 2 };
        byte[] b = { 3, 4, 5 };
        byte[] c = { 6 };
        byte[] result = concat(a, b, c);
        byte[] expected = { 1, 2, 3, 4, 5, 6 };
        if (result.length != expected.length) {
            throw new AssertionError("length mismatch: " + result.length);
        }
        for (int i = 0; i < expected.length; i++) {
            if (result[i] != expected[i]) {
                throw new AssertionError("mismatch at " + i + ": " + result[i]);
            }
        }
        System.out.println("VarargsArrayElementTypeVerifyTest passed!");
    }
}
