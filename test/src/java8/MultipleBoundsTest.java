import java.io.Serializable;

/**
 * Test multiple bounds on type parameters.
 */
public class MultipleBoundsTest {
    
    // Generic method with multiple bounds
    public static <T extends Number & Serializable> double getDouble(T value) {
        return value.doubleValue();
    }
    
    public static void main(String[] args) {
        Integer a = 5;
        double d = MultipleBoundsTest.getDouble(a);
        System.out.println("getDouble(5) = " + d);
        System.out.println("MultipleBoundsTest passed!");
    }
}
