public class AssertHelper {
    public static int divide(int a, int b) {
        assert b != 0;
        return a / b;
    }
    
    public static int divideWithMessage(int a, int b) {
        assert b != 0 : "Divisor cannot be zero";
        return a / b;
    }
    
    public static void checkPositive(int x) {
        assert x > 0 : "Value must be positive: " + x;
    }
}

