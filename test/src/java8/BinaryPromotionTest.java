/*
 * Binary numeric promotion (JLS 5.6.2) between mismatched operand types, and
 * that println picks the overload matching the expression's actual type
 * rather than defaulting to int.
 */
public class BinaryPromotionTest {

    static void check(boolean ok, String what) {
        if (!ok) {
            System.out.println("FAILED: " + what);
            System.exit(1);
        }
    }

    public static void main(String[] args) {
        long a = 1L;
        int i = 2;
        check(a + i == 3L, "long + int");
        check(i + a == 3L, "int + long");

        double d = 1.5;
        check(d + i == 3.5, "double + int");

        long shiftedLeft = a << i;
        check(shiftedLeft == 4L, "long << int");

        // Ternary numeric promotion (JLS 15.25): int/long branches promote to long.
        long picked = true ? i : a;
        check(picked == 2L, "ternary int/long promotes to long");

        java.io.ByteArrayOutputStream bytes = new java.io.ByteArrayOutputStream();
        java.io.PrintStream out = new java.io.PrintStream(bytes);
        out.println(a + i);
        out.println(a < i);
        out.flush();
        String[] lines = bytes.toString().split("\\r?\\n");
        check(lines.length == 2, "println produced two lines");
        check("3".equals(lines[0]), "println selected the long overload, not int: got " + lines[0]);
        check("true".equals(lines[1]), "println selected the boolean overload, not int: got " + lines[1]);

        System.out.println("BinaryPromotionTest passed!");
    }
}
