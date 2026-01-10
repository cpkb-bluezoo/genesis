/**
 * Test static method calls on external classes.
 * 
 * Tests calls like System.exit() where the class name is used directly 
 * for static method calls. This verifies codegen correctly handles
 * static method calls on well-known JDK classes.
 * 
 * Note: Some static methods (Math.abs, Integer.parseInt) require additional
 * semantic analysis support for return type inference. This test focuses on
 * void-returning static methods that are already supported.
 */
public class StaticMethodTest {
    
    public static void main(String[] args) {
        System.out.println("Testing static method calls on external classes...");
        
        // Test System.gc() - void return, no args
        System.gc();
        System.out.println("System.gc() completed");
        
        // Test System.runFinalization() - void return, no args
        System.runFinalization();
        System.out.println("System.runFinalization() completed");
        
        // Test calling a method that will eventually exit
        // We use a helper to test System.exit() indirectly
        testExitMethod();
        
        System.out.println("StaticMethodTest passed!");
    }
    
    private static void testExitMethod() {
        // We can't actually call System.exit(0) without terminating the test,
        // but we can verify the compilation works by having it in dead code
        if (false) {
            System.exit(0);  // This compiles but never executes
        }
        System.out.println("System.exit() compiled successfully (not executed)");
    }
}

