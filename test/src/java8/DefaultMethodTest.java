/**
 * Test for Java 8 default methods in interfaces.
 */
public class DefaultMethodTest {
    
    interface Counter {
        default int getCount() {
            return 42;
        }
        
        default int doubleCount() {
            return getCount() * 2;
        }
    }
    
    static class SimpleCounter implements Counter {
        // Inherits default implementations
    }
    
    static class CustomCounter implements Counter {
        @Override
        public int getCount() {
            return 100;
        }
        // Inherits doubleCount default
    }
    
    public static void main(String[] args) {
        Counter simple = new SimpleCounter();
        Counter custom = new CustomCounter();
        
        // Test default method inheritance
        int result1 = simple.getCount();
        if (result1 != 42) {
            throw new RuntimeException("Expected 42 but got: " + result1);
        }
        
        // Test default method calling another default method
        int result2 = simple.doubleCount();
        if (result2 != 84) {
            throw new RuntimeException("Expected 84 but got: " + result2);
        }
        
        // Test overridden default method
        int result3 = custom.getCount();
        if (result3 != 100) {
            throw new RuntimeException("Expected 100 but got: " + result3);
        }
        
        // Test inherited default method uses overridden method
        int result4 = custom.doubleCount();
        if (result4 != 200) {
            throw new RuntimeException("Expected 200 but got: " + result4);
        }
        
        System.out.println("DefaultMethodTest passed!");
    }
}
