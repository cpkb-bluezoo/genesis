/**
 * Test private methods in interfaces (Java 9 feature).
 * Private interface methods can be used as helpers for default methods.
 */
public class PrivateInterfaceMethodTest {
    
    interface Calculator {
        // Public abstract method
        int calculate(int a, int b);
        
        // Default method that uses a private helper
        default int addWithLogging(int a, int b) {
            log("Adding " + a + " and " + b);
            return a + b;
        }
        
        // Private instance method - helper for default methods
        private void log(String message) {
            System.out.println("[Calculator] " + message);
        }
        
        // Private static method
        private static int abs(int value) {
            return value;  // simplified - just return value
        }
        
        // Static method using private static helper
        static int process(int a) {
            return abs(a);
        }
    }
    
    static class SimpleCalculator implements Calculator {
        public int calculate(int a, int b) {
            return a + b;
        }
    }
    
    public static void main(String[] args) {
        Calculator calc = new SimpleCalculator();
        
        System.out.println("Testing private interface method support:");
        
        // Test default method (which uses private instance helper)
        int sum = calc.addWithLogging(5, 3);
        System.out.println("Sum: " + sum);
        
        // Test static method (which uses private static helper)
        int processed = Calculator.process(42);
        System.out.println("Processed: " + processed);
        
        System.out.println("PASS: Private interface methods work correctly");
    }
}

