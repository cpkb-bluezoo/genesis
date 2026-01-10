/**
 * Test for binary literals (Java 7).
 */
public class BinaryLiteralTest {
    
    public static void main(String[] args) {
        // Test basic binary literals
        int five = 0b101;
        if (five != 5) {
            System.out.println("FAIL: 0b101 should be 5, got " + five);
            return;
        }
        
        // Test uppercase B
        int ten = 0B1010;
        if (ten != 10) {
            System.out.println("FAIL: 0B1010 should be 10, got " + ten);
            return;
        }
        
        // Test with underscores (Java 7 feature)
        int byte_val = 0b1111_0000;
        if (byte_val != 240) {
            System.out.println("FAIL: 0b1111_0000 should be 240, got " + byte_val);
            return;
        }
        
        // Test long binary literal
        long big = 0b1L;
        if (big != 1L) {
            System.out.println("FAIL: 0b1L should be 1, got " + big);
            return;
        }
        
        // Test bitwise operations with binary literals
        int mask = 0b1100 & 0b1010;
        if (mask != 0b1000) {
            System.out.println("FAIL: 0b1100 & 0b1010 should be 0b1000 (8), got " + mask);
            return;
        }
        
        System.out.println("PASS");
    }
}

