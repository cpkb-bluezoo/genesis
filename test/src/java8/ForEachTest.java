/**
 * Test enhanced for loop (for-each) over arrays.
 */
public class ForEachTest {
    
    /**
     * Test basic int array iteration
     */
    public void testIntArray() {
        int[] arr = new int[3];
        arr[0] = 1;
        arr[1] = 2;
        arr[2] = 3;
        
        int sum = 0;
        for (int x : arr) {
            sum = sum + x;
        }
        
        if (sum != 6) {
            throw new RuntimeException("testIntArray failed");
        }
        System.out.println("testIntArray passed");
    }
    
    /**
     * Test String array iteration
     */
    public void testStringArray() {
        String[] arr = new String[3];
        arr[0] = "Hello";
        arr[1] = " ";
        arr[2] = "World";
        
        int count = 0;
        for (String s : arr) {
            count = count + 1;
        }
        
        if (count != 3) {
            throw new RuntimeException("testStringArray failed");
        }
        System.out.println("testStringArray passed");
    }
    
    /**
     * Test with array initializer
     */
    public void testArrayInitializer() {
        int sum = 0;
        int[] nums = {10, 20, 30, 40};
        for (int n : nums) {
            sum = sum + n;
        }
        
        if (sum != 100) {
            throw new RuntimeException("testArrayInitializer failed");
        }
        System.out.println("testArrayInitializer passed");
    }
    
    public static void main(String[] args) {
        ForEachTest test = new ForEachTest();
        test.testIntArray();
        test.testStringArray();
        test.testArrayInitializer();
        System.out.println("ForEachTest passed!");
    }
}
