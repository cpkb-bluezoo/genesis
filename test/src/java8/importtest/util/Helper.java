package importtest.util;

/**
 * Helper class in a separate package for testing cross-package import resolution.
 */
public class Helper {
    public static String getMessage() {
        return "Hello from Helper";
    }
    
    public static int add(int a, int b) {
        return a + b;
    }
}

