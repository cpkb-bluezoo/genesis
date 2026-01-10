public class WideningTest {
    
    public static void main(String[] args) {
        System.out.println("Testing type widening conversions...");
        
        // int + long -> long
        int i = 10;
        long l = 20L;
        long r1 = i + l;
        System.out.println("int + long = " + r1);  // Should be 30
        
        // int * long -> long
        long r2 = i * l;
        System.out.println("int * long = " + r2);  // Should be 200
        
        // int + double -> double
        double d = 3.5;
        double r3 = i + d;
        System.out.println("int + double = " + r3);  // Should be 13.5
        
        // long + float -> float
        float f = 2.5f;
        float r4 = l + f;
        System.out.println("long + float = " + r4);  // Should be 22.5
        
        // float + double -> double
        double r5 = f + d;
        System.out.println("float + double = " + r5);  // Should be 6.0
        
        // Mixed in single expression: int + long + double
        double r6 = i + l + d;
        System.out.println("int + long + double = " + r6);  // Should be 33.5
        
        System.out.println("All widening tests passed!");
    }
}

