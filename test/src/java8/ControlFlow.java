/**
 * Test control flow statements.
 */
public class ControlFlow {
    
    public static int abs(int x) {
        if (x < 0) {
            return x * -1;
        }
        return x;
    }
    
    public static int max(int a, int b) {
        if (a > b) {
            return a;
        } else {
            return b;
        }
    }
    
    public static int factorial(int n) {
        int result = 1;
        int i = 1;
        while (i <= n) {
            result = result * i;
            i = i + 1;
        }
        return result;
    }
    
    public static int sum(int n) {
        int result = 0;
        int i = 1;
        while (i <= n) {
            result = result + i;
            i = i + 1;
        }
        return result;
    }
}

