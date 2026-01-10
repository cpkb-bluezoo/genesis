/*
 * Test for multi-catch (Java 7)
 * Tests catching multiple exception types in a single catch block
 */
public class MultiCatchTest {
    
    static void throwArithmetic() {
        int x = 1 / 0;
    }
    
    static void throwNullPointer() {
        Object o = null;
        o.toString();  /* Will throw NullPointerException */
    }
    
    static void throwArrayIndex() {
        int[] arr = new int[1];
        int x = arr[10];
    }
    
    public static void main(String[] args) {
        int caught = 0;
        
        /* Test 1: Catch ArithmeticException via multi-catch */
        try {
            throwArithmetic();
        } catch (ArithmeticException | NullPointerException e) {
            caught = 1;
        }
        
        if (caught != 1) {
            System.out.println("FAIL: ArithmeticException not caught");
            return;
        }
        
        /* Test 2: Catch NullPointerException via multi-catch */
        caught = 0;
        try {
            throwNullPointer();
        } catch (ArithmeticException | NullPointerException e) {
            caught = 2;
        }
        
        if (caught != 2) {
            System.out.println("FAIL: NullPointerException not caught");
            return;
        }
        
        /* Test 3: Three exception types */
        caught = 0;
        try {
            throwArrayIndex();
        } catch (ArithmeticException | NullPointerException | ArrayIndexOutOfBoundsException e) {
            caught = 3;
        }
        
        if (caught != 3) {
            System.out.println("FAIL: ArrayIndexOutOfBoundsException not caught");
            return;
        }
        
        /* Test 4: Exception not in multi-catch should propagate */
        caught = 0;
        try {
            try {
                throwArrayIndex();
            } catch (ArithmeticException | NullPointerException e) {
                caught = 4;
            }
        } catch (ArrayIndexOutOfBoundsException e) {
            caught = 5;
        }
        
        if (caught != 5) {
            System.out.println("FAIL: Exception should have propagated, caught=" + caught);
            return;
        }
        
        System.out.println("PASS");
    }
}

