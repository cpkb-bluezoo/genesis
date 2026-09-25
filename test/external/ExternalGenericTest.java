/*
 * Generic classes loaded from class files (not source): constructor
 * descriptors must use the erased bound, and fields declared with a type
 * variable must be cast back to the specific type when read.
 */
import lib.Cell;
import lib.NumCell;

public class ExternalGenericTest {
    public static void main(String[] args) {
        Cell<String> c = new Cell<String>("hello");
        int len = c.value.length();
        NumCell<Integer> nc = new NumCell<Integer>(5);
        int five = nc.n.intValue();
        Integer boxed = nc.n;
        nc.set(7);
        int seven = nc.get().intValue();
        if (seven != 7) {
            System.out.println("FAILED: set/get");
            System.exit(1);
        }
        if (len != 5 || five != 5 || boxed.intValue() != 5) {
            System.out.println("FAILED");
            System.exit(1);
        }
        System.out.println("ExternalGenericTest passed!");
    }
}
