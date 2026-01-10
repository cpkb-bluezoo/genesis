/**
 * Test comparison operators for all numeric types.
 * Tests: ==, !=, <, >, <=, >= for int, long, float, double
 */
public class NumericCompareTest {
    
    public static void main(String[] args) {
        testIntCompare();
        testLongCompare();
        testFloatCompare();
        testDoubleCompare();
        testMixedCompare();
        System.out.println("NumericCompareTest passed!");
    }
    
    private static void testIntCompare() {
        int a = 10;
        int b = 20;
        int c = 10;
        
        // Equality
        if (a != c) throw new RuntimeException("int != failed");
        if (a == b) throw new RuntimeException("int == failed");
        
        // Less than
        if (a >= b) throw new RuntimeException("int < failed");
        if (b < a) throw new RuntimeException("int < reverse failed");
        
        // Greater than
        if (b <= a) throw new RuntimeException("int > failed");
        if (a > b) throw new RuntimeException("int > reverse failed");
        
        // Less or equal
        if (a > b) throw new RuntimeException("int <= failed");
        if (a > c) throw new RuntimeException("int <= equal failed");
        
        // Greater or equal
        if (b < a) throw new RuntimeException("int >= failed");
        if (a < c) throw new RuntimeException("int >= equal failed");
        
        System.out.println("int comparisons: OK");
    }
    
    private static void testLongCompare() {
        long a = 1234567890123L;
        long b = 9876543210987L;
        long c = 1234567890123L;
        
        // Equality
        if (a != c) throw new RuntimeException("long != failed");
        if (a == b) throw new RuntimeException("long == failed");
        
        // Less than
        if (a >= b) throw new RuntimeException("long < failed");
        if (b < a) throw new RuntimeException("long < reverse failed");
        
        // Greater than
        if (b <= a) throw new RuntimeException("long > failed");
        if (a > b) throw new RuntimeException("long > reverse failed");
        
        // Less or equal
        if (a > b) throw new RuntimeException("long <= failed");
        if (a > c) throw new RuntimeException("long <= equal failed");
        
        // Greater or equal
        if (b < a) throw new RuntimeException("long >= failed");
        if (a < c) throw new RuntimeException("long >= equal failed");
        
        System.out.println("long comparisons: OK");
    }
    
    private static void testFloatCompare() {
        float a = 3.14f;
        float b = 2.718f;
        float c = 3.14f;
        
        // Equality
        if (a != c) throw new RuntimeException("float != failed");
        if (a == b) throw new RuntimeException("float == failed");
        
        // Less than (b < a is true since 2.718 < 3.14)
        if (b >= a) throw new RuntimeException("float < failed");
        if (a < b) throw new RuntimeException("float < reverse failed");
        
        // Greater than (a > b is true since 3.14 > 2.718)
        if (a <= b) throw new RuntimeException("float > failed");
        if (b > a) throw new RuntimeException("float > reverse failed");
        
        // Less or equal
        if (b > a) throw new RuntimeException("float <= failed");
        if (a > c) throw new RuntimeException("float <= equal failed");
        
        // Greater or equal
        if (a < b) throw new RuntimeException("float >= failed");
        if (a < c) throw new RuntimeException("float >= equal failed");
        
        System.out.println("float comparisons: OK");
    }
    
    private static void testDoubleCompare() {
        double a = 3.141592653589793;
        double b = 2.718281828459045;
        double c = 3.141592653589793;
        
        // Equality
        if (a != c) throw new RuntimeException("double != failed");
        if (a == b) throw new RuntimeException("double == failed");
        
        // Less than (b < a is true)
        if (b >= a) throw new RuntimeException("double < failed");
        if (a < b) throw new RuntimeException("double < reverse failed");
        
        // Greater than (a > b is true)
        if (a <= b) throw new RuntimeException("double > failed");
        if (b > a) throw new RuntimeException("double > reverse failed");
        
        // Less or equal
        if (b > a) throw new RuntimeException("double <= failed");
        if (a > c) throw new RuntimeException("double <= equal failed");
        
        // Greater or equal
        if (a < b) throw new RuntimeException("double >= failed");
        if (a < c) throw new RuntimeException("double >= equal failed");
        
        System.out.println("double comparisons: OK");
    }
    
    private static void testMixedCompare() {
        // Test mixed type comparisons (with widening)
        int i = 10;
        long l = 10L;
        float f = 10.0f;
        double d = 10.0;
        
        // int vs long (widened to long)
        if (i != l) throw new RuntimeException("int/long compare failed");
        
        // int vs float (widened to float)
        if (i != f) throw new RuntimeException("int/float compare failed");
        
        // int vs double (widened to double)
        if (i != d) throw new RuntimeException("int/double compare failed");
        
        // long vs double (widened to double)
        if (l != d) throw new RuntimeException("long/double compare failed");
        
        // float vs double (widened to double)
        if (f != d) throw new RuntimeException("float/double compare failed");
        
        System.out.println("mixed comparisons: OK");
    }
}
