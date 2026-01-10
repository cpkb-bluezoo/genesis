/**
 * Test static initializers and static field initializers.
 */
public class StaticInitTest {
    
    // Static field with initializer
    static int count = 10;
    
    // Static initializer block
    static {
        System.out.println("Static block executed!");
    }
    
    public static void main(String[] args) {
        System.out.println("count = " + count);
        System.out.println("StaticInitTest passed!");
    }
}
