/* Regression test: an anonymous class's own FIELD INITIALIZER reading a
 * local variable of the enclosing method (a capture) - not only its method
 * bodies. genesis only scanned method bodies for captured locals, so the
 * initializer failed with "codegen: cannot resolve identifier" and the
 * class's constructor failed verification. Mirrors gumdrop's
 * TaglibRegistryTest ("byte[] jarBytes = bos.toByteArray();" in an
 * anonymous MapHandler). */
public class AnonymousFieldInitCaptureVerifyTest {
    interface Source {
        Object get();
    }

    static String describe(final String prefix, final int n) {
        Source s = new Source() {
            final String label = prefix + "-" + n;
            int[] counts = new int[] { n, n + 1 };

            @Override
            public Object get() {
                return label + ":" + counts[1];
            }
        };
        return (String) s.get();
    }

    public static void main(String[] args) {
        String got = describe("x", 4);
        if (!"x-4:5".equals(got)) {
            throw new RuntimeException("expected x-4:5 but got " + got);
        }
        System.out.println("AnonymousFieldInitCaptureVerifyTest passed!");
    }
}
