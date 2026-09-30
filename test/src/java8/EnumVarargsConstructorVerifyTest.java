/* Regression test: an enum whose constructor takes a varargs parameter.
 * The enum's static initializer matched the constructor by exact arity
 * and pushed each constant's arguments as written, so "A("a"), B" against
 * "Plain(String... labels)" passed a bare String - or nothing at all -
 * where the constructor takes a String[] ("VerifyError: Bad type on
 * operand stack" in <clinit>). */
public class EnumVarargsConstructorVerifyTest {

    enum Plain {
        A("a"), B, C("x", "y");

        final int n;

        Plain(String... labels) {
            n = labels.length;
        }
    }

    enum Mixed {
        A(1, "a"), B(2), C(3, "x", "y");

        final int n;

        Mixed(int k, String... labels) {
            n = k * 10 + labels.length;
        }
    }

    enum Primitive {
        NONE, ONE(7), MANY(1, 2, 3);

        final int sum;

        Primitive(int... values) {
            int s = 0;
            for (int v : values) {
                s += v;
            }
            sum = s;
        }
    }

    enum Overloaded {
        FIXED(5), VAR("p", "q"), EMPTY;

        final String how;

        Overloaded(int n) {
            how = "int" + n;
        }

        Overloaded(String... parts) {
            how = "varargs" + parts.length;
        }
    }

    private static void check(boolean ok, String what) {
        if (!ok) {
            throw new RuntimeException("failed: " + what);
        }
    }

    public static void main(String[] args) {
        check(Plain.A.n == 1 && Plain.B.n == 0 && Plain.C.n == 2, "String... only");
        check(Mixed.A.n == 11 && Mixed.B.n == 20 && Mixed.C.n == 32, "fixed + String...");
        check(Primitive.NONE.sum == 0 && Primitive.ONE.sum == 7 && Primitive.MANY.sum == 6, "int...");
        check("int5".equals(Overloaded.FIXED.how), "fixed-arity overload preferred, got " + Overloaded.FIXED.how);
        check("varargs2".equals(Overloaded.VAR.how), "varargs overload, got " + Overloaded.VAR.how);
        check("varargs0".equals(Overloaded.EMPTY.how), "varargs overload with no arguments, got " + Overloaded.EMPTY.how);
        check(Plain.values().length == 3 && Plain.valueOf("C") == Plain.C, "values/valueOf");
        System.out.println("PASS");
    }
}
