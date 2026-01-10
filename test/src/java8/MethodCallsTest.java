/**
 * Test method invocation.
 */
public class MethodCallsTest {
    
    public static int staticAdd(int a, int b) {
        return a + b;
    }
    
    public int instanceAdd(int a, int b) {
        return a + b;
    }
    
    public static void main(String[] args) {
        // Static method call
        int x = staticAdd(10, 20);
        
        // Instance method call
        MethodCallsTest obj = new MethodCallsTest();
        int y = obj.instanceAdd(5, 7);
    }
}
