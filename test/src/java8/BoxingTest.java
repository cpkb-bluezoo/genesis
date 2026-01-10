public class BoxingTest {
    
    public void testIntegerBoxing() {
        // Test int -> Integer boxing
        int i = 42;
        Integer boxed = i;
        
        // Test Integer -> int unboxing
        int unboxed = boxed;
        
        if (unboxed != 42) {
            throw new RuntimeException("testIntegerBoxing failed");
        }
        System.out.println("testIntegerBoxing passed");
    }
    
    public void testBooleanBoxing() {
        // Test boolean -> Boolean boxing
        boolean b = true;
        Boolean boxed = b;
        
        // Test Boolean -> boolean unboxing
        boolean unboxed = boxed;
        
        if (!unboxed) {
            throw new RuntimeException("testBooleanBoxing failed");
        }
        System.out.println("testBooleanBoxing passed");
    }
    
    // Helper method that takes Integer parameter
    public int processInteger(Integer value) {
        return value.intValue() + 10;
    }
    
    // Helper method that takes int parameter  
    public int processInt(int value) {
        return value + 20;
    }
    
    public void testMethodArgBoxing() {
        // Test boxing in method argument: int -> Integer
        int primitiveArg = 50;
        int result = processInteger(primitiveArg);
        
        if (result != 60) {
            throw new RuntimeException("testMethodArgBoxing failed");
        }
        System.out.println("testMethodArgBoxing passed");
    }
    
    public void testMethodArgUnboxing() {
        // Test unboxing in method argument: Integer -> int
        int i = 100;
        Integer boxedArg = i;
        int result = processInt(boxedArg);
        
        if (result != 120) {
            throw new RuntimeException("testMethodArgUnboxing failed");
        }
        System.out.println("testMethodArgUnboxing passed");
    }
    
    public static void main(String[] args) {
        BoxingTest test = new BoxingTest();
        test.testIntegerBoxing();
        test.testBooleanBoxing();
        test.testMethodArgBoxing();
        test.testMethodArgUnboxing();
        System.out.println("BoxingTest passed!");
    }
}
