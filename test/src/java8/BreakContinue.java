/**
 * Test break and continue statements.
 */
public class BreakContinue {
    
    // Find first divisor of n (other than 1)
    public static int firstDivisor(int n) {
        int i = 2;
        while (i < n) {
            if (n % i == 0) {
                break;
            }
            i = i + 1;
        }
        return i;
    }
    
    // Sum of odd numbers from 1 to n
    public static int sumOdd(int n) {
        int sum = 0;
        int i = 1;
        while (i <= n) {
            if (i % 2 == 0) {
                i = i + 1;
                continue;
            }
            sum = sum + i;
            i = i + 1;
        }
        return sum;
    }
    
    // Do-while test: count digits
    public static int countDigits(int n) {
        int count = 0;
        do {
            count = count + 1;
            n = n / 10;
        } while (n > 0);
        return count;
    }
}

