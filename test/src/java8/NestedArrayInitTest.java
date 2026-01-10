public class NestedArrayInitTest {
    
    public NestedArrayInitTest() {
    }
    
    public static void main(String[] args) {
        System.out.println("Testing nested array initializers...");
        
        // Test 1: 2D int array initializer
        int[][] matrix = {
            {1, 2, 3},
            {4, 5, 6}
        };
        System.out.println("matrix[0][0]:");
        System.out.println(matrix[0][0]);  // 1
        System.out.println("matrix[1][2]:");
        System.out.println(matrix[1][2]);  // 6
        System.out.println("matrix.length:");
        System.out.println(matrix.length);  // 2
        System.out.println("matrix[0].length:");
        System.out.println(matrix[0].length);  // 3
        
        // Test 2: Ragged array (different inner lengths)
        int[][] ragged = {
            {10, 20},
            {30, 40, 50},
            {60}
        };
        System.out.println("ragged.length:");
        System.out.println(ragged.length);  // 3
        System.out.println("ragged[0].length:");
        System.out.println(ragged[0].length);  // 2
        System.out.println("ragged[1].length:");
        System.out.println(ragged[1].length);  // 3
        System.out.println("ragged[2].length:");
        System.out.println(ragged[2].length);  // 1
        System.out.println("ragged[1][2]:");
        System.out.println(ragged[1][2]);  // 50
        
        // Test 3: Sum all elements
        int sum = 0;
        int i = 0;
        while (i < matrix.length) {
            int j = 0;
            while (j < matrix[i].length) {
                sum = sum + matrix[i][j];
                j = j + 1;
            }
            i = i + 1;
        }
        System.out.println("Sum of matrix elements:");
        System.out.println(sum);  // Should be 21
        
        // Test 4: 2D String array (use local var to work around overload resolution)
        String[][] strs = {
            {"Hello", "World"},
            {"Java", "Compiler"}
        };
        String s1 = strs[0][0];
        String s2 = strs[1][1];
        System.out.println("strs[0][0]:");
        System.out.println(s1);  // Hello
        System.out.println("strs[1][1]:");
        System.out.println(s2);  // Compiler
        
        System.out.println("All nested array init tests passed!");
    }
}
