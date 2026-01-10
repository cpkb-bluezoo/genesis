public class CastInstanceofTest {
    
    public CastInstanceofTest() {
    }
    
    public static void main(String[] args) {
        System.out.println("Testing instanceof and cast expressions...");
        
        // Test 1: instanceof with Object
        Object obj = new Object();
        boolean isObj = obj instanceof Object;
        System.out.println("obj instanceof Object:");
        System.out.println(isObj ? 1 : 0);  // Should be 1
        
        // Test 2: instanceof with String
        Object str = "hello";
        boolean isStr = str instanceof String;
        System.out.println("str instanceof String:");
        System.out.println(isStr ? 1 : 0);  // Should be 1
        
        // Test 3: instanceof negative case
        boolean isStrObj = obj instanceof String;
        System.out.println("obj instanceof String:");
        System.out.println(isStrObj ? 1 : 0);  // Should be 0
        
        // Test 4: Primitive cast int to byte (result stored as int)
        int big = 300;
        int b = (byte) big;
        System.out.println("(byte) 300:");
        System.out.println(b);  // Should be 44 (300 mod 256 = 44)
        
        // Test 5: Primitive cast int to char (result stored as int)
        int charCode = 65;
        int c = (char) charCode;
        System.out.println("(char) 65:");
        System.out.println(c);  // Should be 65
        
        // Test 6: Primitive cast int to short (result stored as int)
        int largeInt = 40000;
        int s = (short) largeInt;
        System.out.println("(short) 40000:");
        System.out.println(s);  // Should be -25536 (overflow)
        
        // Test 7: instanceof with null
        String nullStr = null;
        boolean isNullStr = nullStr instanceof String;
        System.out.println("null instanceof String:");
        System.out.println(isNullStr ? 1 : 0);  // Should be 0
        
        // Test 8: instanceof in condition
        Object maybeStr = "test";
        if (maybeStr instanceof String) {
            System.out.println("maybeStr is a String");
        }
        
        // Test 9: Reference cast (Object to String)
        Object strObj = "world";
        Object castResult = (String) strObj;
        System.out.println("Reference cast succeeded");
        
        // Test 10: Cast in expression (no variable storage)
        int x = 1000;
        System.out.println("(byte) 1000 directly:");
        System.out.println((byte) x);  // Should print -24
        
        System.out.println("All instanceof/cast tests passed!");
    }
}
