/**
 * Test type checking during semantic analysis.
 * This test verifies that valid code compiles and runs correctly.
 * Invalid code (type errors) would fail at compile time.
 */
public class TypeCheckTest {
    
    public static void main(String[] args) {
        // Test method argument types
        testMethodArgs();
        
        // Test boolean conditions
        testBooleanConditions();
        
        // Test array indexing
        testArrayIndex();
        
        // Test varargs
        testVarargs();
        
        System.out.println("TypeCheckTest passed!");
    }
    
    // Method argument type checking
    private static void testMethodArgs() {
        takeInt(42);
        takeString("hello");
        takeBoth(10, "world");
        System.out.println("Method argument types: OK");
    }
    
    private static void takeInt(int x) {
        if (x != 42) {
            throw new RuntimeException("takeInt failed");
        }
    }
    
    private static void takeString(String s) {
        if (s == null) {
            throw new RuntimeException("takeString failed");
        }
    }
    
    private static void takeBoth(int x, String s) {
        if (x != 10) {
            throw new RuntimeException("takeBoth int failed");
        }
        if (s == null) {
            throw new RuntimeException("takeBoth string failed");
        }
    }
    
    // Boolean condition type checking
    private static void testBooleanConditions() {
        // If condition
        boolean flag = true;
        if (flag) {
            System.out.println("If condition works");
        }
        
        // While condition
        int count = 0;
        while (count < 3) {
            count = count + 1;
        }
        if (count != 3) {
            throw new RuntimeException("while loop failed");
        }
        
        // For condition
        int sum = 0;
        for (int i = 0; i < 5; i++) {
            sum = sum + i;
        }
        if (sum != 10) {
            throw new RuntimeException("for loop failed");
        }
        
        // Ternary condition
        int result = flag ? 1 : 0;
        if (result != 1) {
            throw new RuntimeException("ternary failed");
        }
        
        System.out.println("Boolean conditions: OK");
    }
    
    // Array index type checking
    private static void testArrayIndex() {
        int[] arr = new int[5];
        arr[0] = 10;
        arr[1] = 20;
        arr[2] = 30;
        
        // Integer indices work
        int idx = 2;
        if (arr[idx] != 30) {
            throw new RuntimeException("array index failed");
        }
        
        System.out.println("Array indexing: OK");
    }
    
    // Varargs type checking
    private static void testVarargs() {
        // Empty varargs
        varargMethod();
        
        // Single vararg
        varargMethod("a");
        
        // Multiple varargs
        varargMethod("a", "b", "c");
        
        // Array to varargs
        String[] arr = new String[2];
        arr[0] = "x";
        arr[1] = "y";
        varargMethod(arr);
        
        System.out.println("Varargs: OK");
    }
    
    private static void varargMethod(String... args) {
        // Just verify we received the args
        if (args == null) {
            throw new RuntimeException("varargs null");
        }
    }
}

