/**
 * Bug: assigning through ONE level of a multi-dimensional array (e.g.
 * "byte[][] values = ...; values[i] = someByteArray;") emitted the LEAF
 * element type's SCALAR store opcode (BASTORE, for a byte[][]'s ultimate
 * "byte" element type) instead of AASTORE - even though indexing a
 * multi-dimensional array just once still yields a sub-ARRAY (a
 * reference type), not the leaf scalar. Root cause: the array-assignment
 * codegen read the target array's sem_type as {element_type: byte,
 * dimensions: 2} and used element_type->kind directly, ignoring
 * dimensions entirely - array literal initializers (codegen_array_init())
 * already correctly special-case "dimensions > 1 means AASTORE", but the
 * general assignment codegen for "arr[i] = value" never did.
 * VerifyError: "Bad type on operand stack ... '[B' ... not assignable to
 * integer" at the bastore. Confirmed against gumdrop's own
 * MessageIndexEntry.buildVariableData(), whose
 * "values[DESC_LOCATION] = toBytes(location);" on a byte[][] is exactly
 * this shape.
 */
public class MultiDimArrayElementStoreVerifyTest {
    static byte[][] values;

    static void init(int n) {
        values = new byte[n][0];
    }

    static void set(int i, byte[] v) {
        values[i] = v;
    }

    public static void main(String[] args) {
        init(3);
        set(0, new byte[]{1, 2, 3});
        set(1, new byte[]{4, 5});
        if (values[0].length != 3) {
            throw new RuntimeException("expected length 3, got " + values[0].length);
        }
        if (values[1].length != 2) {
            throw new RuntimeException("expected length 2, got " + values[1].length);
        }
        if (values[0][0] != 1 || values[0][2] != 3) {
            throw new RuntimeException("unexpected contents");
        }
    }
}
