// Simple lambda test without imports
public class SimpleLambdaTest {
    @FunctionalInterface
    interface StringLength {
        int length(String s);
    }
    
    public static void main(String[] args) {
        StringLength fn = s -> s.length();
        int len = fn.length("Hello");
        System.out.println("Length: " + len);
    }
}
