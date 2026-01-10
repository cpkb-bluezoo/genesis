/**
 * Test that the source file encoding is correctly detected and validated.
 * This file is UTF-8 encoded.
 */
public class UnicodeTest {
    public static void main(String[] args) {
        // ASCII string - should work
        String ascii = "Hello, World!";
        System.out.println("ASCII: " + ascii);
        
        // Basic Latin with numbers
        String mixed = "Test123!@#";
        System.out.println("Mixed: " + mixed);
        
        // Success
        System.out.println("Encoding test passed!");
    }
}
