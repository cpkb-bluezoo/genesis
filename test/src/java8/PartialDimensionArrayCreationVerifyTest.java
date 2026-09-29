/**
 * Bug: "new Type[n][]" (the outer dimension sized, inner left empty/
 * unallocated) used NEWARRAY on the LEAF element type instead of
 * ANEWARRAY on the remaining array component type - producing a bare
 * "[B" instead of "[[B" for "new byte[n][]". Root cause: the parser
 * already tracks the TRUE total dimension count (2, here) separately
 * from how many dimensions got an actual size expression (1, here -
 * parser.c's own comment: "empty brackets (new byte[3][]) don't add
 * children but still add dimensions"), stashed in the AST node's own
 * flags field - but codegen only ever looked at the number of size-
 * expression children, never that total. Combined with the sibling
 * AASTORE-vs-BASTORE fix (MultiDimArrayElementStoreVerifyTest), this
 * left the CREATED array typed as "[B" when everything downstream
 * expected "[[B": VerifyError the moment a 2-D operation (e.g. storing
 * a byte[] into one slot) exposed the mismatch. Confirmed against
 * gumdrop's own MessageIndexEntry, whose "byte[][] values = new
 * byte[DESCRIPTOR_COUNT][];" is exactly this shape.
 */
public class PartialDimensionArrayCreationVerifyTest {
    public static void main(String[] args) {
        byte[][] values = new byte[3][];
        values[0] = new byte[]{1, 2, 3};
        values[1] = new byte[]{4, 5};
        values[2] = new byte[0];

        if (values.length != 3) {
            throw new RuntimeException("expected outer length 3, got " + values.length);
        }
        if (values[0].length != 3 || values[0][2] != 3) {
            throw new RuntimeException("unexpected values[0]");
        }
        if (values[1].length != 2) {
            throw new RuntimeException("unexpected values[1]");
        }
        if (values[2].length != 0) {
            throw new RuntimeException("unexpected values[2]");
        }
    }
}
