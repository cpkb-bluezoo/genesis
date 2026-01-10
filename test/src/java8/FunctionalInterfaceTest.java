/**
 * Test @FunctionalInterface annotation validation.
 * These interfaces should all compile successfully because they each
 * have exactly one abstract method.
 */
public class FunctionalInterfaceTest {
    
    // Valid functional interface - exactly one abstract method
    @FunctionalInterface
    interface Processor {
        void process(String input);
    }
    
    // Valid functional interface - one abstract, one default
    @FunctionalInterface
    interface Transformer {
        String transform(String input);
        
        default String identity(String input) {
            return input;
        }
    }
    
    // Valid functional interface - one abstract, one static
    @FunctionalInterface
    interface Calculator {
        int calculate(int a, int b);
        
        static int helper(int x) {
            return x * 2;
        }
    }
    
    public static void main(String[] args) {
        System.out.println("FunctionalInterface validation test");
        System.out.println("Test passed!");
    }
}

