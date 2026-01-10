/**
 * Test for Java 16+ pattern matching for instanceof.
 */
public class PatternMatchTest {
    public static void main(String[] args) {
        // Test with String
        Object obj1 = "Hello, World!";
        if (obj1 instanceof String s) {
            System.out.println("obj1 is String: " + s);
            System.out.println("Length: " + s.length());
        }
        
        // Test with another String
        Object obj2 = "Test";
        if (obj2 instanceof String str) {
            System.out.println("obj2 is String: " + str);
            System.out.println("Length2: " + str.length());
        }
        
        // Test negative case - use a non-String object
        Object obj3 = new StringBuilder("builder");
        if (obj3 instanceof String s) {
            System.out.println("obj3 is String: " + s);  // Should not print
        } else {
            System.out.println("obj3 is not a String (correct!)");
        }
        
        // Test with null (instanceof with null returns false)
        Object obj4 = null;
        if (obj4 instanceof String s) {
            System.out.println("obj4 is String: " + s);  // Should not print
        } else {
            System.out.println("obj4 is null, not a String (correct!)");
        }
        
        // Test with helper method
        testPattern("Test String");
        testPattern(new StringBuilder("not a string"));
        
        System.out.println("All pattern matching tests passed!");
    }
    
    static void testPattern(Object obj) {
        if (obj instanceof String str) {
            System.out.println("Method arg is String: " + str);
        } else {
            System.out.println("Method arg is not a String");
        }
    }
}

