public class LongDoubleTest {
    
    public static void main(String[] args) {
        System.out.println("Testing long and double local variables...");
        
        // Test long
        long x = 1234567890123L;
        long y = 9876543210987L;
        long sum = x + y;
        System.out.println("Long sum test passed");
        
        // Test double
        double a = 3.14159265358979;
        double b = 2.71828182845904;
        double product = a * b;
        System.out.println("Double product test passed");
        
        // Test float
        float f1 = 1.5f;
        float f2 = 2.5f;
        float fsum = f1 + f2;
        System.out.println("Float sum test passed");
        
        // Test int (for reference)
        int i1 = 100;
        int i2 = 200;
        int isum = i1 + i2;
        System.out.println("Int sum test passed");
        
        System.out.println("All long/double tests passed!");
    }
}
