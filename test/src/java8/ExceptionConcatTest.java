public class ExceptionConcatTest {
    public static void main(String[] args) {
        // Test 1: String + int
        int intVal = 42;
        try {
            throw new RuntimeException("int value: " + intVal);
        } catch (RuntimeException e) {
            String msg = e.getMessage();
            System.out.println("Test 1 (string+int): " + msg);
        }
        
        // Test 2: String + String
        String strVal = "hello";
        try {
            throw new RuntimeException("string value: " + strVal);
        } catch (RuntimeException e) {
            String msg = e.getMessage();
            System.out.println("Test 2 (string+string): " + msg);
        }
        
        // Test 3: String + long
        long longVal = 9876543210L;
        try {
            throw new RuntimeException("long value: " + longVal);
        } catch (RuntimeException e) {
            String msg = e.getMessage();
            System.out.println("Test 3 (string+long): " + msg);
        }
        
        // Test 4: String + double
        double doubleVal = 3.14159;
        try {
            throw new RuntimeException("double value: " + doubleVal);
        } catch (RuntimeException e) {
            String msg = e.getMessage();
            System.out.println("Test 4 (string+double): " + msg);
        }
        
        // Test 5: String + boolean
        boolean boolVal = true;
        try {
            throw new RuntimeException("bool value: " + boolVal);
        } catch (RuntimeException e) {
            String msg = e.getMessage();
            System.out.println("Test 5 (string+boolean): " + msg);
        }
        
        // Test 6: String + char
        char charVal = 'X';
        try {
            throw new RuntimeException("char value: " + charVal);
        } catch (RuntimeException e) {
            String msg = e.getMessage();
            System.out.println("Test 6 (string+char): " + msg);
        }
        
        // Test 7: Multiple concatenations
        int a = 1;
        int b = 2;
        int c = 3;
        try {
            throw new RuntimeException("values: " + a + ", " + b + ", " + c);
        } catch (RuntimeException e) {
            String msg = e.getMessage();
            System.out.println("Test 7 (multiple): " + msg);
        }
        
        System.out.println("ExceptionConcatTest passed!");
    }
}
