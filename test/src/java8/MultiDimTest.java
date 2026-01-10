public class MultiDimTest {
    
    public MultiDimTest() {
    }
    
    public static void main(String[] args) {
        System.out.println("Testing multi-dimensional arrays...");
        
        // Test 1: Create 2D int array
        int[][] arr = new int[2][3];
        System.out.println("Created int[2][3]");
        
        // Test 2: Outer length
        System.out.println("arr.length:");
        System.out.println(arr.length);  // Should be 2
        
        // Test 3: Inner length
        System.out.println("arr[0].length:");
        System.out.println(arr[0].length);  // Should be 3
        
        // Test 4: Element assignment
        arr[0][0] = 1;
        arr[0][1] = 2;
        arr[0][2] = 3;
        arr[1][0] = 4;
        arr[1][1] = 5;
        arr[1][2] = 6;
        
        // Test 5: Element access
        System.out.println("arr[0][0]:");
        System.out.println(arr[0][0]);  // Should be 1
        System.out.println("arr[1][2]:");
        System.out.println(arr[1][2]);  // Should be 6
        
        // Test 6: Sum all elements
        int sum = 0;
        int i = 0;
        while (i < arr.length) {
            int j = 0;
            while (j < arr[i].length) {
                sum = sum + arr[i][j];
                j = j + 1;
            }
            i = i + 1;
        }
        System.out.println("Sum of all elements:");
        System.out.println(sum);  // Should be 21
        
        System.out.println("All multi-dimensional tests passed!");
    }
}
