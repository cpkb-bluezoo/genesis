/* Must NOT compile: a mutable field is not a constant, and a constant whose
 * value does not fit the target type cannot be narrowed (JLS 5.2). Valid
 * narrowing of constants is covered by NarrowingConstantFromClassfileTest. */
public class NarrowingRejected {
    static int mutable = 3;
    static final int WIDE = 300;

    public static void main(String[] args) {
        byte bad = mutable;
        byte tooBig = WIDE;
    }
}
