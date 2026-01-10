public class TryCatchHelper {
    
    public static int divideWithCatch(int a, int b) {
        int result = 0;
        try {
            result = a / b;
        } catch (ArithmeticException e) {
            result = -1;
        }
        return result;
    }
    
    public static int divideWithFinally(int a, int b) {
        int result = 0;
        try {
            result = a / b;
        } finally {
            result = result + 100;
        }
        return result;
    }
    
    public static int divideWithCatchFinally(int a, int b) {
        int result = 0;
        try {
            result = a / b;
        } catch (ArithmeticException e) {
            result = -1;
        } finally {
            result = result + 1000;
        }
        return result;
    }
}
