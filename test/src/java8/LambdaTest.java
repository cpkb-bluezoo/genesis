// Lambda expression tests with custom functional interfaces
public class LambdaTest {
    
    @FunctionalInterface
    interface StringToInt {
        int apply(String s);
    }
    
    @FunctionalInterface
    interface Supplier {
        String get();
    }
    
    @FunctionalInterface
    interface Consumer {
        void accept(String s);
    }
    
    @FunctionalInterface
    interface IntToString {
        String apply(int n);
    }
    
    @FunctionalInterface
    interface BinaryOp {
        int compute(int a, int b);
    }
    
    public static void main(String[] args) {
        // Test 1: Simple lambda with no captures
        StringToInt strlen = s -> s.length();
        int len = strlen.apply("Hello");
        System.out.println("Length of 'Hello': " + len);
        
        // Test 2: Lambda with no parameters (Supplier)
        Supplier hello = () -> "Hello, Lambda!";
        System.out.println(hello.get());
        
        // Test 3: Lambda with void return (Consumer)
        Consumer printer = msg -> System.out.println(msg);
        printer.accept("Consumer test message");
        
        // Test 4: Lambda capturing local variable
        String prefix = "Result: ";
        IntToString format = n -> prefix + n;
        System.out.println(format.apply(42));
        
        // Test 5: Test non-capturing lambda after capturing one
        Supplier suffix = () -> "Done!";
        System.out.println(suffix.get());
        
        // Test 6: Lambda with multiple parameters
        BinaryOp add = (a, b) -> a + b;
        System.out.println("3 + 5 = " + add.compute(3, 5));
        
        System.out.println("Lambda test completed!");
    }
}
