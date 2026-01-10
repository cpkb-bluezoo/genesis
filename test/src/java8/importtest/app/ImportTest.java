package importtest.app;

import importtest.util.Helper;

/**
 * Test import resolution from a different package.
 * This test verifies that classes defined in other source files
 * with package declarations can be found and used via import statements.
 */
public class ImportTest {
    public static void main(String[] args) {
        // Test static method call on imported class
        String msg = Helper.getMessage();
        if (msg == null) {
            throw new RuntimeException("FAILED: getMessage() returned null");
        }
        System.out.println("Message: " + msg);
        
        // Test static method with parameters
        int sum = Helper.add(10, 32);
        if (sum != 42) {
            throw new RuntimeException("FAILED: add(10, 32) returned: " + sum);
        }
        System.out.println("Sum: " + sum);
        
        System.out.println("ImportTest passed!");
    }
}

