/**
 * Test internal static method calls.
 */
public class InternalCalls {
    
    public static int add(int a, int b) {
        return a + b;
    }
    
    public static int addThree(int a, int b, int c) {
        int ab = add(a, b);
        return add(ab, c);
    }
}

