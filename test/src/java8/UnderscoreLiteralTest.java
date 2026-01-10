/**
 * Test for underscores in numeric literals (Java 7).
 */
public class UnderscoreLiteralTest {
    
    public static void main(String[] args) {
        // Test underscores in decimal integers
        int million = 1_000_000;
        if (million != 1000000) {
            System.out.println("FAIL: 1_000_000 should be 1000000, got " + million);
            return;
        }
        
        // Test underscores in hex literals
        int hex = 0xFF_FF_FF_FF;
        if (hex != -1) {  // 0xFFFFFFFF as signed int is -1
            System.out.println("FAIL: 0xFF_FF_FF_FF should be -1, got " + hex);
            return;
        }
        
        // Test underscores in binary literals
        int binary = 0b1010_1010;
        if (binary != 170) {
            System.out.println("FAIL: 0b1010_1010 should be 170, got " + binary);
            return;
        }
        
        // Test underscores in long literals
        long big = 999_999_999_999L;
        if (big != 999999999999L) {
            System.out.println("FAIL: 999_999_999_999L incorrect");
            return;
        }
        
        // Test underscores in floating point literals
        double pi = 3.14_15_92;
        if (pi < 3.14 || pi > 3.15) {
            System.out.println("FAIL: 3.14_15_92 should be ~3.141592, got " + pi);
            return;
        }
        
        // Test underscores in float literals
        float f = 1_234.5_6f;
        if (f < 1234.5f || f > 1234.6f) {
            System.out.println("FAIL: 1_234.5_6f incorrect");
            return;
        }
        
        // Test multiple consecutive underscores
        int multi = 1__000___000;
        if (multi != 1000000) {
            System.out.println("FAIL: 1__000___000 should be 1000000, got " + multi);
            return;
        }
        
        System.out.println("PASS");
    }
}

