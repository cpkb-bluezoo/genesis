/**
 * Test instance initializers.
 */
public class InstanceInitTest {
    
    // Instance field with initializer
    int count = 10;
    
    // Instance initializer block
    {
        System.out.println("Instance init block executed!");
    }
    
    public InstanceInitTest() {
        System.out.println("Constructor called");
    }
    
    public static void main(String[] args) {
        System.out.println("Creating instance...");
        InstanceInitTest obj = new InstanceInitTest();
        System.out.println("count = " + obj.count);
        System.out.println("InstanceInitTest passed!");
    }
}
