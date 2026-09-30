/* Regression test: a varargs CONSTRUCTOR whose element type is a
 * primitive or an array. The constructor-call path carried its own copy
 * of the varargs packaging logic that built an Object[] for any element
 * type that was not a class, so "new Box(1, 2, 3)" against "Box(int...
 * xs)" stored raw ints with AASTORE ("VerifyError: Bad type on operand
 * stack"), and it only passed an existing array straight through for
 * String/Object elements. Method calls already handled all of these;
 * both now share one implementation. */
public class VarargsConstructorElementTypesVerifyTest {

    static final class Box {
        final String s;

        Box(int... xs) {
            s = "I" + xs.length;
        }

        Box(String tag, byte[]... parts) {
            s = tag + parts.length;
        }

        Box(long scale, String... names) {
            s = "S" + names.length;
        }

        Box(char c, double... ds) {
            double sum = 0;
            for (double d : ds) {
                sum += d;
            }
            s = c + String.valueOf((int) sum);
        }
    }

    private static void check(String expected, Box box, String what) {
        if (!expected.equals(box.s)) {
            throw new RuntimeException(what + ": expected " + expected + ", got " + box.s);
        }
    }

    public static void main(String[] args) {
        byte[] one = { 1 };
        byte[][] both = { one, one };
        int[] nums = { 1, 2, 3, 4 };
        String[] names = { "x", "y" };

        check("I3", new Box(1, 2, 3), "int...");
        check("I0", new Box(), "int... with no arguments");
        check("I4", new Box(nums), "int[] passed through");
        check("B1", new Box("B", one), "one byte[]");
        check("B3", new Box("B", one, one, one), "three byte[]");
        check("B2", new Box("B", both), "byte[][] passed through");
        check("B0", new Box("B"), "byte[]... with no arguments");
        check("S2", new Box(2L, "a", "b"), "String...");
        check("S2", new Box(2L, names), "String[] passed through");
        check("S0", new Box(2L), "String... with no arguments");
        check("d6", new Box('d', 1.5, 2.5, 2), "double... with an int widened");
        System.out.println("PASS");
    }
}
