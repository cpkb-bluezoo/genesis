import java.util.Comparator;

/*
 * Regression test: an anonymous class implementing a generic interface
 * method instantiated with an ARRAY type (e.g. "Comparator<byte[]>",
 * whose erased "compare(Object, Object)" bridge must forward to the
 * concrete "compare(byte[], byte[])") - matching gumdrop's own
 * DnssecValidator, which sorts NSEC records with an anonymous
 * Comparator<byte[]>. Used to throw at class-verification time:
 *
 *   java.lang.VerifyError: Bad type on operand stack
 *   Reason: Type 'java/lang/Object' (current frame, stack[N]) is not
 *   assignable to '[B'
 *
 * Root cause: codegen.c's generate_interface_bridges() already handles
 * casting a bridge's erased Object argument down to the implementation's
 * real parameter type when that type is a TYPE_CLASS (e.g.
 * "Comparator<String>") - but had no equivalent branch for a TYPE_ARRAY
 * implementation parameter (e.g. "byte[]"), so the bridge's
 * compare(Object, Object) forwarded its arguments straight into
 * compare([B, [B) with no CHECKCAST at all.
 *
 * Fixed by adding a TYPE_ARRAY branch alongside the existing TYPE_CLASS
 * one, using the array's own full JVM descriptor (e.g. "[B") as the
 * CHECKCAST class constant per JVMS 4.4.1, mirroring the array-CHECKCAST
 * convention already used elsewhere in the same function.
 */
public class ComparatorArrayTypeVarBridgeVerifyTest {
    static Comparator<byte[]> COMPARATOR = new Comparator<byte[]>() {
        @Override
        public int compare(byte[] a, byte[] b) {
            int len = Math.min(a.length, b.length);
            for (int i = 0; i < len; i++) {
                int diff = (a[i] & 0xFF) - (b[i] & 0xFF);
                if (diff != 0) {
                    return diff;
                }
            }
            return a.length - b.length;
        }
    };

    public static void main(String[] args) {
        byte[] a = {1, 2, 3};
        byte[] b = {1, 2, 4};
        byte[] c = {1, 2, 3};

        int ab = COMPARATOR.compare(a, b);
        if (ab >= 0) {
            throw new RuntimeException("expected a < b, got " + ab);
        }

        int ac = COMPARATOR.compare(a, c);
        if (ac != 0) {
            throw new RuntimeException("expected a == c, got " + ac);
        }

        /* Also exercise it through the erased Comparator interface type,
         * to force the bridge's own compare(Object, Object) path. */
        Comparator<byte[]> asInterface = COMPARATOR;
        int ba = asInterface.compare(b, a);
        if (ba <= 0) {
            throw new RuntimeException("expected b > a, got " + ba);
        }

        System.out.println("ComparatorArrayTypeVarBridgeVerifyTest passed!");
    }
}
