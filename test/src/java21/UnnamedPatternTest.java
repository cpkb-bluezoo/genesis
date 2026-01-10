public class UnnamedPatternTest {
    
    public static void main(String[] args) {
        testStandaloneUnnamed();
        testUnnamedTypePattern();
        testMixedPatterns();
        
        System.out.println("All unnamed pattern tests passed!");
    }
    
    // Test standalone unnamed pattern: case _ ->
    static void testStandaloneUnnamed() {
        String result = formatObject("hello");
        if (!result.equals("String value")) {
            throw new RuntimeException("Failed for String: " + result);
        }
        
        result = formatObject(new Object());
        if (!result.equals("Something else")) {
            throw new RuntimeException("Failed for Object: " + result);
        }
    }
    
    static String formatObject(Object obj) {
        return switch (obj) {
            case String s -> "String value";
            case _ -> "Something else";  // Matches anything else
        };
    }
    
    // Test unnamed type pattern: case Type _ ->
    static void testUnnamedTypePattern() {
        String result = describeType("world");
        if (!result.equals("Is a String")) {
            throw new RuntimeException("Failed describeType for String: " + result);
        }
        
        result = describeType(new Object());
        if (!result.equals("Other type")) {
            throw new RuntimeException("Failed describeType for Object: " + result);
        }
    }
    
    static String describeType(Object obj) {
        return switch (obj) {
            case String _ -> "Is a String";   // Match String but ignore value
            default -> "Other type";
        };
    }
    
    // Test mixed patterns (named and unnamed)
    static void testMixedPatterns() {
        String result = processValue("hello");
        if (!result.equals("String: hello")) {
            throw new RuntimeException("testMixedPatterns failed for String: " + result);
        }
        
        Object o = new Object();
        result = processValue(o);
        if (!result.equals("Unknown")) {
            throw new RuntimeException("testMixedPatterns failed for Object: " + result);
        }
    }
    
    static String processValue(Object obj) {
        return switch (obj) {
            case String s -> "String: " + s;   // Named - we use the value
            case _ -> "Unknown";               // Catch all - unnamed
        };
    }
}
