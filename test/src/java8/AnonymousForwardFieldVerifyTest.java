/* Regression test: a method of an anonymous class naming a field declared
 * LATER in the same anonymous class body (legal: a method body sees every
 * member of its class, JLS 6.3), where the field's initializer also reads
 * a captured local. genesis resolved anonymous-class members strictly in
 * declaration order, so the method body failed with "codegen: cannot
 * resolve identifier" for the forward-declared field. Mirrors gumdrop's
 * TaglibRegistryTest (anonymous MapHandler with a trailing jarBytes
 * field). */
public class AnonymousForwardFieldVerifyTest {
    interface Source {
        Object get();
    }

    public static void main(String[] args) {
        final int seed = 41;
        Source s = new Source() {
            @Override
            public Object get() {
                return data;
            }

            int[] data = new int[] { seed + 1 };
        };
        int v = ((int[]) s.get())[0];
        if (v != 42) {
            throw new RuntimeException("expected 42 but got " + v);
        }
        System.out.println("AnonymousForwardFieldVerifyTest passed!");
    }
}
