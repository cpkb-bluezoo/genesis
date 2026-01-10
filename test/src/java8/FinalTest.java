/**
 * Test final variable checking.
 * This test verifies that final variables work correctly (legal cases).
 * 
 * Illegal cases to test separately:
 *   - Extending a final class
 *   - Overriding a final method
 *   - Reassigning a final variable
 *   - Reassigning a final field
 *   - Reassigning a final parameter
 */

final class FinalClass {
    public int getValue() {
        return 42;
    }
}

class BaseWithFinalMethod {
    public final int getFinalValue() {
        return 100;
    }
    
    public int getRegularValue() {
        return 200;
    }
}

class SubClass extends BaseWithFinalMethod {
    // Can override non-final method
    public int getRegularValue() {
        return 300;
    }
}

public class FinalTest {
    // Final field with initializer
    private final int finalField = 10;
    
    // Final static field
    private static final int CONSTANT = 42;
    
    // Final field initialized in constructor
    private final int constructorInit;
    
    public FinalTest() {
        this.constructorInit = 20;  // OK - initializing in constructor
    }
    
    public int getFinalField() {
        return finalField;
    }
    
    public int getConstructorInit() {
        return constructorInit;
    }
    
    public static void main(String[] args) {
        // Test final class instantiation
        FinalClass fc = new FinalClass();
        System.out.println("Final class value: " + fc.getValue());
        
        // Test final method inheritance
        SubClass sc = new SubClass();
        System.out.println("Final method: " + sc.getFinalValue());
        System.out.println("Overridden method: " + sc.getRegularValue());
        
        // Test final fields
        FinalTest ft = new FinalTest();
        System.out.println("Final field: " + ft.getFinalField());
        System.out.println("Constructor init: " + ft.getConstructorInit());
        
        // Test final static constant
        System.out.println("Constant: " + CONSTANT);
        
        // Test final local variable
        final int localFinal = 5;
        System.out.println("Final local: " + localFinal);
        
        // Test final parameter in lambda/method (we use a helper method)
        testFinalParam(99);
        
        System.out.println("FinalTest passed!");
    }
    
    public static void testFinalParam(final int param) {
        System.out.println("Final param: " + param);
        // Cannot reassign param here - would be an error
    }
}

