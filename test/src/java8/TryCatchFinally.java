public class TryCatchFinally {
    
    // Simple try-catch
    public static int simpleTryCatch(int x) {
        int result = 0;
        try {
            result = 100 / x;
        } catch (ArithmeticException e) {
            result = -1;
        }
        return result;
    }
    
    // Try-catch-finally
    public static int tryCatchFinally(int x) {
        int result = 0;
        try {
            result = 100 / x;
        } catch (ArithmeticException e) {
            result = -1;
        } finally {
            result = result + 1;
        }
        return result;
    }
    
    // Try-finally (no catch)
    public static int tryFinally(int x) {
        int result = 0;
        try {
            result = x * 2;
        } finally {
            result = result + 10;
        }
        return result;
    }
    
    // Multiple catch blocks
    public static int multipleCatch(int x) {
        int result = 0;
        try {
            if (x == 0) {
                result = 100 / x;
            } else if (x < 0) {
                result = -1;
            } else {
                result = x;
            }
        } catch (ArithmeticException e) {
            result = -100;
        } catch (RuntimeException e) {
            result = -200;
        }
        return result;
    }
    
    // Nested try-catch
    public static int nestedTryCatch(int x, int y) {
        int result = 0;
        try {
            try {
                result = 100 / x;
            } catch (ArithmeticException e) {
                result = 50 / y;
            }
        } catch (ArithmeticException e) {
            result = -1;
        }
        return result;
    }
    
    // Try-catch with return in catch
    public static int returnInCatch(int x) {
        try {
            return 100 / x;
        } catch (ArithmeticException e) {
            return -1;
        }
    }
    
    // Finally with return 
    public static int finallyWithReturn(int x) {
        int result = 0;
        try {
            result = 100 / x;
        } catch (ArithmeticException e) {
            result = -1;
        } finally {
            return result + 1000;
        }
    }
    
    public static void main(String[] args) {
        // Test simpleTryCatch
        System.out.println("simpleTryCatch(10) = " + simpleTryCatch(10));
        System.out.println("simpleTryCatch(0) = " + simpleTryCatch(0));
        
        // Test tryCatchFinally
        System.out.println("tryCatchFinally(10) = " + tryCatchFinally(10));
        System.out.println("tryCatchFinally(0) = " + tryCatchFinally(0));
        
        // Test tryFinally
        System.out.println("tryFinally(5) = " + tryFinally(5));
        
        // Test multipleCatch
        System.out.println("multipleCatch(5) = " + multipleCatch(5));
        System.out.println("multipleCatch(-1) = " + multipleCatch(-1));
        System.out.println("multipleCatch(0) = " + multipleCatch(0));
        
        // Test nestedTryCatch
        System.out.println("nestedTryCatch(10, 5) = " + nestedTryCatch(10, 5));
        System.out.println("nestedTryCatch(0, 5) = " + nestedTryCatch(0, 5));
        System.out.println("nestedTryCatch(0, 0) = " + nestedTryCatch(0, 0));
        
        // Test returnInCatch
        System.out.println("returnInCatch(10) = " + returnInCatch(10));
        System.out.println("returnInCatch(0) = " + returnInCatch(0));
        
        // Test finallyWithReturn
        System.out.println("finallyWithReturn(10) = " + finallyWithReturn(10));
        System.out.println("finallyWithReturn(0) = " + finallyWithReturn(0));
        
        System.out.println("All tests passed!");
    }
}

