import java.io.ByteArrayInputStream;
import java.io.FilterInputStream;
import java.io.IOException;

/*
 * "new FilterInputStream(new ByteArrayInputStream(payload)) { ... }" -
 * FilterInputStream's real constructor takes InputStream, but the
 * argument's own static type is the narrower ByteArrayInputStream.
 * codegen_anonymous_class() (codegen.c) built the invokespecial
 * descriptor for the super() call by re-deriving each argument's type
 * from the argument EXPRESSION itself, instead of using the ACTUALLY
 * DECLARED parameter type of the superclass constructor semantic
 * analysis resolved - invokespecial resolves by exact descriptor match,
 * not by mere assignability, so this referenced a constructor overload
 * that doesn't exist: NoSuchMethodError
 * 'FilterInputStream.<init>(ByteArrayInputStream)' at runtime (the real
 * one takes InputStream).
 *
 * Root cause (two parts):
 *  1. A classfile-loaded superclass (FilterInputStream) has its members
 *     populated lazily via a completer that simply hadn't run yet at the
 *     point semantic.c tried to look up its constructors for this
 *     anonymous class - the lookup was silently skipped entirely
 *     (data.class_data.members was still NULL), so no constructor
 *     overload was ever resolved. Fixed by calling symbol_complete()
 *     (idempotent) on the superclass symbol before the lookup.
 *  2. Once resolved, the chosen constructor was stored on the
 *     AST_NEW_OBJECT expression's own sem_symbol - but codegen reached
 *     for it via anon_sym->ast->sem_symbol, and an earlier pre-scan pass
 *     can point anon_sym->ast at the class BODY block instead (well
 *     before the expression is even visited), leaving that chain
 *     pointing at the class symbol itself, not the constructor. Fixed by
 *     storing the resolved constructor directly on a dedicated
 *     class_data.resolved_super_ctor field instead.
 *
 * Confirmed against gumdrop's own DefaultServletCopyBufferTest.
 */
public class AnonSuperCtorWideningArgVerifyTest {
    public static void main(String[] args) throws IOException {
        final int payloadSize = 4;
        final int[] largestRead = { 0 };
        byte[] payload = new byte[] { 1, 2, 3, 4 };
        FilterInputStream in = new FilterInputStream(new ByteArrayInputStream(payload)) {
            @Override
            public int available() {
                return payloadSize;
            }

            @Override
            public int read(byte[] b, int off, int len) throws IOException {
                int n = super.read(b, off, len);
                if (n > 0) {
                    largestRead[0] = Math.max(largestRead[0], n);
                }
                return n;
            }
        };
        byte[] buf = new byte[8];
        int n = in.read(buf, 0, 8);
        if (n != 4 || largestRead[0] != 4) {
            throw new RuntimeException("expected n=4 largestRead=4, got n=" + n + " largestRead=" + largestRead[0]);
        }
        System.out.println("AnonSuperCtorWideningArgVerifyTest passed!");
    }
}
