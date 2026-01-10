/**
 * Test import resolution from sourcepath.
 * This test verifies that classes defined in other source files
 * on the sourcepath can be found and used.
 */
public class SourcepathTest {
    public static void main(String[] args) {
        // Test static method call on external class
        String msg = SourcepathHelper.getStaticMessage();
        if (msg == null) {
            throw new RuntimeException("FAILED: getStaticMessage() returned null");
        }
        System.out.println("Static method: " + msg);
        
        // Test static method with parameters
        int sum = SourcepathHelper.add(10, 32);
        if (sum != 42) {
            throw new RuntimeException("FAILED: add(10, 32) returned: " + sum);
        }
        System.out.println("Static add: " + sum);
        
        System.out.println("SourcepathTest passed!");
    }
}

