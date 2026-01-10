public class ArrayInitTest {
    
    public ArrayInitTest() {
    }
    
    public static void main(String[] args) {
        System.out.println("Testing array initializers...");
        
        // Test 1: int array initializer
        int[] nums = new int[]{10, 20, 30, 40, 50};
        System.out.println("int[] = {10, 20, 30, 40, 50}");
        System.out.println("nums[0]:");
        System.out.println(nums[0]);  // Should be 10
        System.out.println("nums[2]:");
        System.out.println(nums[2]);  // Should be 30
        System.out.println("nums[4]:");
        System.out.println(nums[4]);  // Should be 50
        System.out.println("nums.length:");
        System.out.println(nums.length);  // Should be 5
        
        // Test 2: Sum of initialized array
        int sum = 0;
        int i = 0;
        while (i < nums.length) {
            sum = sum + nums[i];
            i = i + 1;
        }
        System.out.println("Sum:");
        System.out.println(sum);  // Should be 150
        
        // Test 3: boolean array initializer
        boolean[] flags = new boolean[]{true, false, true};
        System.out.println("boolean[] = {true, false, true}");
        System.out.println("flags[0]:");
        System.out.println(flags[0] ? 1 : 0);  // Should be 1
        System.out.println("flags[1]:");
        System.out.println(flags[1] ? 1 : 0);  // Should be 0
        System.out.println("flags[2]:");
        System.out.println(flags[2] ? 1 : 0);  // Should be 1
        
        // Test 4: String array initializer (via local vars)
        String[] strs = new String[]{"Hello", "World", "!"};
        String s0 = strs[0];
        String s1 = strs[1];
        String s2 = strs[2];
        System.out.println("String[] = {Hello, World, !}");
        System.out.println(s0);
        System.out.println(s1);
        System.out.println(s2);
        
        // Test 5: Empty array
        int[] empty = new int[]{};
        System.out.println("Empty array length:");
        System.out.println(empty.length);  // Should be 0
        
        // Test 6: Single element
        int[] single = new int[]{42};
        System.out.println("Single element:");
        System.out.println(single[0]);  // Should be 42
        
        // Test 7: Larger array (tests bipush)
        int[] large = new int[]{1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
        System.out.println("large.length:");
        System.out.println(large.length);  // Should be 10
        System.out.println("large[9]:");
        System.out.println(large[9]);  // Should be 10
        
        System.out.println("All array initializer tests passed!");
    }
}

