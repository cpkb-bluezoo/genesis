import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;

/* Regression test: a method reference to a variable-arity method whose functional interface passes a
 * primitive array (OpenJDK's MethodReference36). "int[]" is not "Object[]", so the reference is a
 * variable-arity call: the array is one element of the packed one. genesis handed the array to the
 * metafactory as it was, which refused it ("[C is not convertible to [Ljava.lang.Object;"). An
 * Object[] or Integer[] is an Object[], and still goes through as the argument itself. */
public class VarargsMethodRefPrimitiveArrayTest {
    static int length;
    static Object first;

    interface SamC { void m(char[] a); }
    interface SamZ { void m(boolean[] a); }
    interface SamB { void m(byte[] a); }
    interface SamS { void m(short[] a); }
    interface SamI { void m(int[] a); }
    interface SamL { void m(long[] a); }
    interface SamF { void m(float[] a); }
    interface SamD { void m(double[] a); }
    interface SamO { void m(Object[] a); }
    interface SamInteger { void m(Integer[] a); }
    interface SamNested { void m(int[][] a); }
    interface SamTwo { void m(String a, String b); }
    interface SamNone { void m(); }

    static void m(Object... vi) {
        length = vi.length;
        first = vi.length > 0 ? vi[0] : null;
    }

    static int sum(int... values) {
        int s = 0;
        for (int v : values) {
            s += v;
        }
        return s;
    }

    static int count(Object... values) {
        return values.length;
    }

    static String join(String... parts) {
        return String.join("+", parts);
    }

    int base = 100;

    int plus(Object... values) {
        return base + values.length;
    }

    private static void check(boolean ok, String what) {
        if (!ok) {
            System.out.println("FAILED: " + what);
            System.exit(1);
        }
    }

    public static void main(String[] args) {
        SamC sc = VarargsMethodRefPrimitiveArrayTest::m;
        sc.m(new char[] {'a', 'b'});
        check(length == 1 && first instanceof char[], "char[] is one element");

        SamZ sz = VarargsMethodRefPrimitiveArrayTest::m;
        sz.m(new boolean[] {true, false});
        check(length == 1 && first instanceof boolean[], "boolean[] is one element");

        SamB sb = VarargsMethodRefPrimitiveArrayTest::m;
        sb.m(new byte[] {0, 1});
        check(length == 1 && first instanceof byte[], "byte[] is one element");

        SamS ss = VarargsMethodRefPrimitiveArrayTest::m;
        ss.m(new short[] {0, 1});
        check(length == 1 && first instanceof short[], "short[] is one element");

        SamI si = VarargsMethodRefPrimitiveArrayTest::m;
        si.m(new int[] {0, 1});
        check(length == 1 && first instanceof int[], "int[] is one element");

        SamL sl = VarargsMethodRefPrimitiveArrayTest::m;
        sl.m(new long[] {0, 1});
        check(length == 1 && first instanceof long[], "long[] is one element");

        SamF sf = VarargsMethodRefPrimitiveArrayTest::m;
        sf.m(new float[] {0, 1});
        check(length == 1 && first instanceof float[], "float[] is one element");

        SamD sd = VarargsMethodRefPrimitiveArrayTest::m;
        sd.m(new double[] {0, 1});
        check(length == 1 && first instanceof double[], "double[] is one element");

        SamO so = VarargsMethodRefPrimitiveArrayTest::m;
        so.m(new Object[] {"x", "y"});
        check(length == 2 && "x".equals(first), "Object[] is the array itself");

        SamInteger sInteger = VarargsMethodRefPrimitiveArrayTest::m;
        sInteger.m(new Integer[] {1, 2, 3});
        check(length == 3, "Integer[] is the array itself");

        SamNested sn = VarargsMethodRefPrimitiveArrayTest::m;
        sn.m(new int[][] {{1}, {2}});
        check(length == 2 && first instanceof int[], "int[][] is an Object[]");

        Function<int[], Integer> counted = VarargsMethodRefPrimitiveArrayTest::count;
        check(counted.apply(new int[] {1, 2, 3}) == 1, "a primitive array to a Function");

        Function<int[], Integer> summed = VarargsMethodRefPrimitiveArrayTest::sum;
        check(summed.apply(new int[] {1, 2, 3}) == 6, "int[] to int... is the array itself");

        VarargsMethodRefPrimitiveArrayTest self = new VarargsMethodRefPrimitiveArrayTest();
        Function<long[], Integer> bound = self::plus;
        check(bound.apply(new long[] {1, 2}) == 101, "a bound reference");

        BiFunction<VarargsMethodRefPrimitiveArrayTest, byte[], Integer> unbound = VarargsMethodRefPrimitiveArrayTest::plus;
        check(unbound.apply(self, new byte[] {1, 2, 3}) == 101, "an unbound reference");

        SamTwo two = VarargsMethodRefPrimitiveArrayTest::m;
        two.m("a", "b");
        check(length == 2, "loose arguments are packed");

        SamNone none = VarargsMethodRefPrimitiveArrayTest::m;
        none.m();
        check(length == 0, "no arguments is an empty array");

        Supplier<String> joined = VarargsMethodRefPrimitiveArrayTest::join;
        check(joined.get().isEmpty(), "an empty String...");
        Function<String, String> one = VarargsMethodRefPrimitiveArrayTest::join;
        check(one.apply("solo").equals("solo"), "one String for String...");
        Function<String[], String> direct = VarargsMethodRefPrimitiveArrayTest::join;
        check(direct.apply(new String[] {"p", "q"}).equals("p+q"), "String[] to String... is the array itself");

        System.out.println("VarargsMethodRefPrimitiveArrayTest passed!");
    }
}
