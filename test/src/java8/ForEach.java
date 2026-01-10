/**
 * Test enhanced for loop (for-each).
 */
public class ForEach {
    
    public static int sumArray(int[] arr) {
        int sum = 0;
        for (int x : arr) {
            sum = sum + x;
        }
        return sum;
    }
    
    public static int countPositive(int[] arr) {
        int count = 0;
        for (int x : arr) {
            if (x > 0) {
                count = count + 1;
            }
        }
        return count;
    }
    
    public static int product(int[] arr) {
        int result = 1;
        for (int x : arr) {
            result = result * x;
        }
        return result;
    }
}

