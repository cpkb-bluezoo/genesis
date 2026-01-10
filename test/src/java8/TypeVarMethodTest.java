/**
 * Test method calls on type variable receivers.
 * Validates that methods can be called on variables of type variable type,
 * using the bound type for method resolution.
 */
public class TypeVarMethodTest {
    public static void main(String[] args) {
        // Test calling method on bounded type variable using autoboxing
        int x = 42;
        Integer i = x;  // Autoboxing
        int result = getIntValue(i);
        if (result != 42) {
            throw new RuntimeException("Test 1 failed: expected 42, got " + result);
        }
        
        // Test with another value
        int y = 100;
        Integer j = y;  // Autoboxing
        result = getIntValue(j);
        if (result != 100) {
            throw new RuntimeException("Test 2 failed: expected 100, got " + result);
        }
        
        System.out.println("TypeVarMethodTest passed!");
    }
    
    /**
     * Generic method with Number bound - calls intValue() on the bound type.
     */
    static <E extends Number> int getIntValue(E num) {
        return num.intValue();
    }
}

