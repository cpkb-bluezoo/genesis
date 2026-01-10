public class OperatorTest {
    
    public static void main(String[] args) {
        System.out.println("Testing operators...");
        
        // Arithmetic operators
        int a = 10;
        int b = 3;
        
        System.out.println("a + b:");
        System.out.println(a + b);
        
        System.out.println("a - b:");
        System.out.println(a - b);
        
        System.out.println("a * b:");
        System.out.println(a * b);
        
        System.out.println("a / b:");
        System.out.println(a / b);
        
        System.out.println("a % b:");
        System.out.println(a % b);
        
        // Bitwise operators
        int x = 5;
        int y = 3;
        
        System.out.println("x & y:");
        System.out.println(x & y);
        
        System.out.println("x | y:");
        System.out.println(x | y);
        
        System.out.println("x ^ y:");
        System.out.println(x ^ y);
        
        System.out.println("x << 1:");
        System.out.println(x << 1);
        
        System.out.println("x >> 1:");
        System.out.println(x >> 1);
        
        // Comparison operators
        System.out.println("a == 10:");
        System.out.println(a == 10);
        
        System.out.println("a != 10:");
        System.out.println(a != 10);
        
        System.out.println("a < 15:");
        System.out.println(a < 15);
        
        System.out.println("a > 15:");
        System.out.println(a > 15);
        
        System.out.println("a <= 10:");
        System.out.println(a <= 10);
        
        System.out.println("a >= 10:");
        System.out.println(a >= 10);
        
        // Unary operators
        int c = 5;
        System.out.println("-c:");
        System.out.println(-c);
        
        System.out.println("+c:");
        System.out.println(+c);
        
        boolean t = true;
        System.out.println("!true:");
        System.out.println(!t);
        
        System.out.println("~5:");
        System.out.println(~5);
        
        // Compound assignments
        int d = 10;
        d += 5;
        System.out.println("d += 5:");
        System.out.println(d);
        
        d -= 3;
        System.out.println("d -= 3:");
        System.out.println(d);
        
        d *= 2;
        System.out.println("d *= 2:");
        System.out.println(d);
        
        d /= 4;
        System.out.println("d /= 4:");
        System.out.println(d);
        
        d %= 5;
        System.out.println("d %= 5:");
        System.out.println(d);
        
        int e = 7;
        e &= 3;
        System.out.println("e &= 3:");
        System.out.println(e);
        
        e |= 4;
        System.out.println("e |= 4:");
        System.out.println(e);
        
        e ^= 2;
        System.out.println("e ^= 2:");
        System.out.println(e);
        
        int f = 4;
        f <<= 1;
        System.out.println("f <<= 1:");
        System.out.println(f);
        
        f >>= 1;
        System.out.println("f >>= 1:");
        System.out.println(f);
        
        System.out.println("All operator tests passed!");
    }
}

