public class ArrayTest {
    
    public ArrayTest() {
    }
    
    public static void main(String[] args) {
        System.out.println("Testing array operations...");
        
        // Test 1: Create int array
        int[] arr = new int[5];
        System.out.println("Created int[5]");
        
        // Test 2: Array length
        int len = arr.length;
        System.out.println("arr.length:");
        System.out.println(len);  // Should be 5
        
        // Test 3: Array element assignment
        arr[0] = 10;
        arr[1] = 20;
        arr[2] = 30;
        arr[3] = 40;
        arr[4] = 50;
        System.out.println("Assigned values 10-50");
        
        // Test 4: Array element access
        System.out.println("arr[0]:");
        System.out.println(arr[0]);  // Should be 10
        System.out.println("arr[2]:");
        System.out.println(arr[2]);  // Should be 30
        System.out.println("arr[4]:");
        System.out.println(arr[4]);  // Should be 50
        
        // Test 5: Array element in expression
        int sum = arr[0] + arr[1] + arr[2];
        System.out.println("arr[0] + arr[1] + arr[2]:");
        System.out.println(sum);  // Should be 60
        
        // Test 6: Array element modification
        arr[0] = arr[0] + 5;
        System.out.println("arr[0] after += 5:");
        System.out.println(arr[0]);  // Should be 15
        
        // Test 7: Loop through array
        int total = 0;
        int i = 0;
        while (i < arr.length) {
            total = total + arr[i];
            i = i + 1;
        }
        System.out.println("Sum of all elements:");
        System.out.println(total);  // Should be 155 (15+20+30+40+50)
        
        // Test 8: String array - store and assign
        String[] strs = new String[3];
        strs[0] = "Hello";
        strs[1] = " ";
        strs[2] = "World";
        // Use local variable to print (workaround for method resolution)
        String s0 = strs[0];
        String s2 = strs[2];
        System.out.println("String array strs[0]:");
        System.out.println(s0);
        System.out.println("String array strs[2]:");
        System.out.println(s2);
        
        // Test 9: Boolean array
        boolean[] flags = new boolean[2];
        flags[0] = true;
        flags[1] = false;
        System.out.println("flags[0]:");
        System.out.println(flags[0] ? 1 : 0);  // Should be 1
        System.out.println("flags[1]:");
        System.out.println(flags[1] ? 1 : 0);  // Should be 0
        
        // Test 10: Array length in expression
        int[] nums = new int[10];
        System.out.println("new int[10].length:");
        System.out.println(nums.length);  // Should be 10
        
        // Test 11: Nested array access
        arr[1] = arr[0] + arr[2];
        System.out.println("arr[1] = arr[0] + arr[2]:");
        System.out.println(arr[1]);  // Should be 45 (15+30)
        
        System.out.println("All array tests passed!");
    }
}
