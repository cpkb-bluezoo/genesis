public class ChainedFieldsTest {
    
    public static void main(String[] args) {
        // Test 1: Simple System.out (2-level chain)
        System.out.println("Test 1: System.out.println works");
        
        // Test 2: System.err (also a PrintStream)
        System.err.println("Test 2: System.err.println works");
        
        // Test 3: Multiple println calls
        System.out.println("Test 3a");
        System.out.println("Test 3b");
        System.out.println("Test 3c");
        
        // Test 4: Print different types
        System.out.println(100);
        System.out.println(true);
        System.out.println(false);
        
        // Test 5: Print expressions
        int x = 5;
        int y = 10;
        System.out.println(x);
        System.out.println(y);
        
        System.out.println("All tests passed!");
    }
}

