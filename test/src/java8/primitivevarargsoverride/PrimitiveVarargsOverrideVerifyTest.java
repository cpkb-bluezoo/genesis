package primitivevarargsoverride;

/* Regression test: "@Override" on a method overriding one declared in
 * ANOTHER FILE, where the overridden method's varargs element type is a
 * primitive (int..., long..., boolean..., ...), was rejected with "Method
 * 'sum' does not override a method from its superclass or interfaces".
 * method_ast_matches_signature()'s varargs branch only knew how to name a
 * CLASS element type and returned "no match" for anything else, while the
 * ordinary array branch right below it handled primitives fine. Within
 * one file, or without the annotation, nothing went wrong. */
public class PrimitiveVarargsOverrideVerifyTest extends Base {

    @Override
    public int sum(int... values) {
        int s = 0;
        for (int v : values) {
            s += v;
        }
        return s;
    }

    @Override
    public long total(int scale, long... values) {
        long s = 0;
        for (long v : values) {
            s += v;
        }
        return s * scale;
    }

    @Override
    public int count(boolean... flags) {
        int n = 0;
        for (boolean f : flags) {
            if (f) {
                n++;
            }
        }
        return n;
    }

    @Override
    public String mixed(String prefix, double... values) {
        return prefix + values.length;
    }

    @Override
    public int chars(char... cs) {
        return cs.length;
    }

    @Override
    public int pieces(byte[]... parts) {
        return parts.length;
    }

    @Override
    public int plain(int[] values) {
        return values.length;
    }

    private static void check(boolean ok, String what) {
        if (!ok) {
            throw new RuntimeException("failed: " + what);
        }
    }

    public static void main(String[] args) {
        Base b = new PrimitiveVarargsOverrideVerifyTest();
        check(b.sum(1, 2, 3) == 6, "int...");
        check(b.sum() == 0, "int... with no arguments");
        check(b.total(2, 10L, 20L) == 60L, "long...");
        check(b.count(true, false, true) == 2, "boolean...");
        check("d2".equals(b.mixed("d", 1.5, 2.5)), "double...");
        check(b.chars('a', 'b', 'c') == 3, "char...");
        check(b.pieces(new byte[] { 1 }, new byte[] { 2 }) == 2, "byte[]...");
        check(b.plain(new int[] { 1, 2 }) == 2, "int[]");
        System.out.println("PASS");
    }
}
