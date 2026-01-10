/**
 * Test for Java 8 static methods in interfaces.
 */
public class StaticInterfaceMethodTest {
    
    interface Calculator {
        // Static utility method
        static int add(int a, int b) {
            return a + b;
        }
        
        static int multiply(int a, int b) {
            return a * b;
        }
    }
    
    public static void main(String[] args) {
        // Test static methods on interface directly
        int result1 = Calculator.add(3, 4);
        int result2 = Calculator.multiply(5, 6);
        
        // Simple validation without conditionals
        System.out.println(result1);
        System.out.println(result2);
        System.out.println("StaticInterfaceMethodTest passed!");
    }
}
