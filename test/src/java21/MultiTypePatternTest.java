public class MultiTypePatternTest {
    
    public static void main(String[] args) {
        // Test with multiple type patterns - use new wrappers
        testMultiplePatterns("Hello");
        testMultiplePatterns(new Integer(42));
        testMultiplePatterns(new Double(3.14));
        testMultiplePatterns(new Object());
    }
    
    public static void testMultiplePatterns(Object obj) {
        String result = switch (obj) {
            case String s -> "String: " + s;
            case Integer i -> "Integer: " + i;
            case Double d -> "Double: " + d;
            default -> "Unknown type";
        };
        System.out.println(result);
    }
}
