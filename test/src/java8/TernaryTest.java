public class TernaryTest {
    
    public static void main(String[] args) {
        System.out.println("Testing ternary operator...");
        
        // Test 1: Basic ternary with true condition
        int a = 5;
        int b = 10;
        int max1 = a > b ? a : b;
        System.out.println("max(5, 10):");
        System.out.println(max1);  // Should be 10
        
        // Test 2: Basic ternary with false condition
        int max2 = b > a ? b : a;
        System.out.println("max(10, 5):");
        System.out.println(max2);  // Should be 10
        
        // Test 3: Ternary with equality test
        int x = 5;
        int result1 = x == 5 ? 100 : 200;
        System.out.println("x==5 ? 100 : 200:");
        System.out.println(result1);  // Should be 100
        
        // Test 4: Ternary with inequality test
        int result2 = x != 5 ? 100 : 200;
        System.out.println("x!=5 ? 100 : 200:");
        System.out.println(result2);  // Should be 200
        
        // Test 5: Nested ternary
        int y = 15;
        int result3 = y < 10 ? 1 : y < 20 ? 2 : 3;
        System.out.println("y<10 ? 1 : y<20 ? 2 : 3 (y=15):");
        System.out.println(result3);  // Should be 2
        
        // Test 6: Ternary in expression
        int sum = (a > b ? a : b) + (x == 5 ? 10 : 20);
        System.out.println("max(5,10) + (x==5?10:20):");
        System.out.println(sum);  // Should be 20 (10 + 10)
        
        // Test 7: Ternary with method call arguments
        testPrint(a > b ? a : b);  // Should print 10
        
        // Test 8: Ternary returning boolean as int
        int bool_result = a < b ? 1 : 0;
        System.out.println("a<b as int:");
        System.out.println(bool_result);  // Should be 1
        
        System.out.println("All ternary tests passed!");
    }
    
    public static void testPrint(int val) {
        System.out.println("testPrint received:");
        System.out.println(val);
    }
}

