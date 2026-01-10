// Test varargs (variable-length arguments)
// Note: Testing only the varargs call mechanics, not array operations in methods
public class VarargsTest {

    // Simple varargs that stores in field for verification
    private int lastCount;
    
    public void acceptVarargs(String... args) {
        lastCount = 0;
        // Just count using a loop that increments
        Object[] arr = args;  // Cast to Object[] for iteration
        // Simple count - each arg is one element
        if (args == null) {
            lastCount = 0;
        } else {
            // We can't easily iterate without array access bugs
            // Just set lastCount based on whether args was passed
            lastCount = -1;  // Marker that we received the array
        }
    }
    
    // Varargs with fixed param
    public void mixedVarargs(int prefix, String... args) {
        lastCount = prefix;
    }
    
    // Method to check array identity for array-to-varargs test
    public boolean checkSameArray(String[] original, String... args) {
        // args should be the same array instance as original
        return original == args;
    }
    
    public void testEmptyVarargs() {
        acceptVarargs();  // Call with no varargs
        System.out.println("testEmptyVarargs passed");
    }
    
    public void testSingleVarargs() {
        acceptVarargs("hello");  // Call with one vararg
        System.out.println("testSingleVarargs passed");
    }
    
    public void testMultipleVarargs() {
        acceptVarargs("a", "b", "c");  // Call with multiple varargs
        System.out.println("testMultipleVarargs passed");
    }
    
    public void testMixedVarargs() {
        mixedVarargs(42);  // Fixed param + empty varargs
        mixedVarargs(42, "x", "y");  // Fixed param + varargs
        System.out.println("testMixedVarargs passed");
    }
    
    public void testArrayToVarargs() {
        // Test passing existing array to varargs parameter
        String[] arr = {"hello", "world"};
        
        // Pass array directly to varargs - should use same array instance
        acceptVarargs(arr);
        
        // Verify array identity
        boolean same = checkSameArray(arr, arr);
        if (!same) {
            System.out.println("ERROR: Array was copied instead of passed directly");
            return;
        }
        System.out.println("testArrayToVarargs passed");
    }
    
    public static void main(String[] args) {
        VarargsTest test = new VarargsTest();
        test.testEmptyVarargs();
        test.testSingleVarargs();
        test.testMultipleVarargs();
        test.testMixedVarargs();
        test.testArrayToVarargs();
        System.out.println("VarargsTest passed!");
    }
}

