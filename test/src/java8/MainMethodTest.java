/**
 * Test main method signature validation.
 * 
 * The main method must have the exact signature:
 *   public static void main(String[] args)
 * 
 * This test verifies that a properly declared main method:
 * - Has public access modifier
 * - Is static
 * - Returns void
 * - Accepts a single String[] parameter
 * 
 * Invalid signatures (tested manually, should produce compile errors):
 * - Non-public main: static void main(String[] args)
 * - Non-static main: public void main(String[] args)
 * - Wrong return type: public static int main(String[] args)
 * - Wrong parameter: public static void main(int args)
 * - No parameters: public static void main()
 * - Too many parameters: public static void main(String[] args, int x)
 */
public class MainMethodTest {
    
    public static void main(String[] args) {
        // Verify args is accessible and is a String array
        System.out.println("Args length: " + args.length);
        
        // Test that main method can call other static methods
        int result = helper();
        if (result != 42) {
            throw new RuntimeException("FAILED: helper() returned " + result);
        }
        
        // Test calling another public static method
        testMainOverload();
        
        System.out.println("MainMethodTest passed!");
    }
    
    private static int helper() {
        return 42;
    }
    
    // This is a valid overload of main, but not the entry point
    public static void testMainOverload() {
        System.out.println("Overloaded method called");
    }
}

