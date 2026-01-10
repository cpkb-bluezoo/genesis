public class TestForEach {
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
}

