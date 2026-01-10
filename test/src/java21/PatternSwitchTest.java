/**
 * Test for pattern matching in switch (Java 21).
 */
public class PatternSwitchTest {
    
    public static void main(String[] args) {
        testTypePattern();
        
        System.out.println("All pattern switch tests passed!");
    }
    
    static void testTypePattern() {
        Object obj = "Hello";
        String result = switch (obj) {
            case String s -> s;
            default -> "Unknown";
        };
        
        System.out.println("Type pattern result: " + result);
    }
}

