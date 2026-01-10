// Method reference tests with custom functional interfaces
public class MethodRefTest {
    
    @FunctionalInterface
    interface StringToInt {
        int apply(String s);
    }
    
    @FunctionalInterface
    interface IntToInt {
        int apply(int n);
    }
    
    @FunctionalInterface
    interface Supplier {
        String get();
    }
    
    @FunctionalInterface
    interface Consumer {
        void accept(String s);
    }
    
    // Static method for static method reference test
    public static int stringLength(String s) {
        return s.length();
    }
    
    // Static method for int
    public static int doubleIt(int n) {
        return n * 2;
    }
    
    // Instance method
    public String greet(String name) {
        return "Hello, " + name;
    }
    
    // No-arg instance method
    public String getGreeting() {
        return "Greetings!";
    }
    
    // Instance method for consumer
    public void printMessage(String msg) {
        System.out.println("Instance method: " + msg);
    }
    
    public static void main(String[] args) {
        MethodRefTest instance = new MethodRefTest();
        
        // Test 1: Static method reference - ClassName::staticMethod
        StringToInt strlen = MethodRefTest::stringLength;
        int len = strlen.apply("Hello");
        System.out.println("Static method ref length: " + len);
        
        // Test 2: Static method reference with int
        IntToInt doubler = MethodRefTest::doubleIt;
        int doubled = doubler.apply(21);
        System.out.println("Static method ref doubled: " + doubled);
        
        // Test 3: Bound instance method reference - instance::method
        Supplier supplier = instance::getGreeting;
        System.out.println("Bound instance method ref: " + supplier.get());
        
        // Test 4: Another bound instance reference
        Consumer printer = instance::printMessage;
        printer.accept("Test message");
        
        // Test 5: Instance method reference on String
        StringToInt strLen2 = String::length;
        int len2 = strLen2.apply("World");
        System.out.println("String::length result: " + len2);
        
        System.out.println("Method reference test completed!");
    }
}

