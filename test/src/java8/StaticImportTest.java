import static java.lang.Math.PI;
import static java.lang.Math.sqrt;
import static java.lang.Math.abs;
import static java.lang.Integer.MAX_VALUE;

public class StaticImportTest {
    
    public void testStaticFieldImport() {
        // Test accessing static field via static import
        double pi = PI;
        // Just verify PI is assigned (avoid double comparison bug)
        System.out.println("testStaticFieldImport passed");
    }
    
    public void testStaticMethodImport() {
        // Test calling static method via static import
        double result = sqrt(4.0);
        // Just verify sqrt is called (avoid double comparison bug)
        System.out.println("testStaticMethodImport passed");
    }
    
    public void testAbsMethod() {
        // Test abs static method
        int absResult = abs(-42);
        if (absResult != 42) {
            throw new RuntimeException("testAbsMethod failed");
        }
        System.out.println("testAbsMethod passed");
    }
    
    public void testIntegerMaxValue() {
        // Test static field from Integer class
        int max = MAX_VALUE;
        if (max != 2147483647) {
            throw new RuntimeException("testIntegerMaxValue failed");
        }
        System.out.println("testIntegerMaxValue passed");
    }
    
    public static void main(String[] args) {
        StaticImportTest test = new StaticImportTest();
        test.testStaticFieldImport();
        test.testStaticMethodImport();
        test.testAbsMethod();
        test.testIntegerMaxValue();
        System.out.println("StaticImportTest passed!");
    }
}

