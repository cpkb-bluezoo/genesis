public class IncrementTest {
    
    private int field;
    
    public IncrementTest() {
        field = 0;
    }
    
    public static void main(String[] args) {
        System.out.println("Testing increment/decrement operators...");
        
        // Test 1: Pre-increment on local variable
        int a = 5;
        int b = ++a;
        System.out.println("++a (a was 5):");
        System.out.println(b);  // Should be 6
        System.out.println("a after ++a:");
        System.out.println(a);  // Should be 6
        
        // Test 2: Post-increment on local variable
        int c = 10;
        int d = c++;
        System.out.println("c++ (c was 10):");
        System.out.println(d);  // Should be 10 (old value)
        System.out.println("c after c++:");
        System.out.println(c);  // Should be 11
        
        // Test 3: Pre-decrement on local variable
        int e = 20;
        int f = --e;
        System.out.println("--e (e was 20):");
        System.out.println(f);  // Should be 19
        System.out.println("e after --e:");
        System.out.println(e);  // Should be 19
        
        // Test 4: Post-decrement on local variable
        int g = 30;
        int h = g--;
        System.out.println("g-- (g was 30):");
        System.out.println(h);  // Should be 30 (old value)
        System.out.println("g after g--:");
        System.out.println(g);  // Should be 29
        
        // Test 5: Increment in expression
        int i = 5;
        int j = i++ + 10;
        System.out.println("i++ + 10 (i was 5):");
        System.out.println(j);  // Should be 15 (5 + 10)
        System.out.println("i after:");
        System.out.println(i);  // Should be 6
        
        // Test 6: Pre-increment in expression
        int k = 5;
        int l = ++k + 10;
        System.out.println("++k + 10 (k was 5):");
        System.out.println(l);  // Should be 16 (6 + 10)
        System.out.println("k after:");
        System.out.println(k);  // Should be 6
        
        // Test 7: Multiple increments in same expression
        int m = 5;
        int n = m++ + ++m;
        System.out.println("m++ + ++m (m was 5):");
        System.out.println(n);  // Should be 5 + 7 = 12
        System.out.println("m after:");
        System.out.println(m);  // Should be 7
        
        // Test 8: Increment on instance field
        IncrementTest obj = new IncrementTest();
        obj.testFieldIncrement();
        
        System.out.println("All increment/decrement tests passed!");
    }
    
    public void testFieldIncrement() {
        field = 100;
        
        // Test pre-increment on field
        int x = ++field;
        System.out.println("++field (field was 100):");
        System.out.println(x);  // Should be 101
        System.out.println("field after ++field:");
        System.out.println(field);  // Should be 101
        
        // Test post-increment on field
        int y = field++;
        System.out.println("field++ (field was 101):");
        System.out.println(y);  // Should be 101 (old value)
        System.out.println("field after field++:");
        System.out.println(field);  // Should be 102
        
        // Test pre-decrement on field
        int z = --field;
        System.out.println("--field (field was 102):");
        System.out.println(z);  // Should be 101
        
        // Test post-decrement on field
        int w = field--;
        System.out.println("field-- (field was 101):");
        System.out.println(w);  // Should be 101 (old value)
        System.out.println("field after field--:");
        System.out.println(field);  // Should be 100
    }
}
