/* Regression test: a field, return type or parameter typed N[] (or N[][]),
 * with N extends Number, loaded from a CLASS FILE was referenced as Object[]
 * instead of Number[] (VerifyError: Object is not assignable to Integer when
 * a method was invoked on an element). Bare type variables were already
 * handled; arrays of them were not. */
public class TypeVarArrayFieldTest {
    public static void main(String[] args) {
        NumArrayBox<Integer> b = new NumArrayBox<>(
            new Integer[] {1, 2}, new Integer[][] {{3}, {4, 5}});
        Number first = b.arr[0];
        double d = b.arr[1].doubleValue();
        int i = b.get()[0].intValue();
        int g = b.grid[1][1].intValue();
        double s = b.sum(new Integer[] {3, 4});
        if (first.intValue() != 1 || d != 2.0 || i != 1 || g != 5 || s != 7.0) {
            System.out.println("FAILED");
            System.exit(1);
        }
        System.out.println("TypeVarArrayFieldTest passed!");
    }
}
