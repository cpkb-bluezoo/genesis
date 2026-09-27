/*
 * Implicit widening primitive conversions (JLS 5.1.2) at every context that
 * allows one: local variable declarations, simple assignment (local, field,
 * array element), method return, and a boxing declaration (Object o = 1).
 */
public class PrimitiveWideningTest {

    static long instanceLong;
    static double instanceDouble;

    static long returnLong(int i) {
        return i;
    }

    static Integer returnBoxed(int i) {
        return i;
    }

    static int returnUnboxed(Integer i) {
        return i;
    }

    static void check(boolean ok, String what) {
        if (!ok) {
            System.out.println("FAILED: " + what);
            System.exit(1);
        }
    }

    long field;

    public static void main(String[] args) {
        int i = 5;

        long a = 1;
        check(a == 1L, "long local decl from int literal");

        double d = i;
        check(d == 5.0, "double local decl from int");

        long b;
        b = i;
        check(b == 5L, "long local simple assignment from int");

        long c = i;
        c = 9;
        check(c == 9L, "long local re-assignment from int literal");

        float f = 1L;
        check(f == 1.0f, "float local decl from long");

        instanceLong = i;
        check(instanceLong == 5L, "static long field assignment from int");

        instanceDouble = i;
        check(instanceDouble == 5.0, "static double field assignment from int");

        long[] arr = new long[2];
        arr[0] = i;
        check(arr[0] == 5L, "long array element store from int");

        check(returnLong(i) == 5L, "widening on return");
        check(returnBoxed(i).intValue() == 5, "boxing on return");
        check(returnUnboxed(7) == 7, "unboxing on return");

        Object o = 1;
        check(o instanceof Integer && ((Integer) o).intValue() == 1,
              "boxing a primitive to Object in a declaration");

        PrimitiveWideningTest t = new PrimitiveWideningTest();
        t.field = i;
        check(t.field == 5L, "instance long field assignment from int");

        System.out.println("PrimitiveWideningTest passed!");
    }
}
