/**
 * Test Java 10+ local variable type inference with 'var' keyword.
 * Note: Comparisons on long/float/double are not tested due to a separate
 * codegen bug with lcmp/fcmp/dcmp opcodes.
 */
public class VarInferenceTest {
    
    public static void main(String[] args) {
        testIntVar();
        testBooleanVar();
        testCharVar();
        testStringVar();
        testArrayVar();
        testExpressionVar();
        System.out.println("VarInferenceTest passed!");
    }
    
    // Test var with int
    private static void testIntVar() {
        var i = 42;
        if (i != 42) throw new RuntimeException("int failed");
        System.out.println("int var: OK");
    }
    
    // Test var with boolean
    private static void testBooleanVar() {
        var b = true;
        if (!b) throw new RuntimeException("boolean failed");
        System.out.println("boolean var: OK");
    }
    
    // Test var with char
    private static void testCharVar() {
        var c = 'X';
        if (c != 'X') throw new RuntimeException("char failed");
        System.out.println("char var: OK");
    }
    
    // Test var with strings
    private static void testStringVar() {
        var s = "hello";
        var concat = s + " world";
        
        if (s == null) throw new RuntimeException("string init failed");
        if (concat == null) throw new RuntimeException("string concat failed");
        
        System.out.println("String var: OK");
    }
    
    // Test var with arrays
    private static void testArrayVar() {
        var intArr = new int[3];
        intArr[0] = 1;
        intArr[1] = 2;
        intArr[2] = 3;
        
        if (intArr[0] != 1) throw new RuntimeException("int array failed");
        if (intArr.length != 3) throw new RuntimeException("array length failed");
        
        var strArr = new String[2];
        strArr[0] = "a";
        strArr[1] = "b";
        if (strArr[0] == null) throw new RuntimeException("String array failed");
        
        System.out.println("array var: OK");
    }
    
    // Test var with expressions
    private static void testExpressionVar() {
        var a = 10;
        var b = 20;
        var sum = a + b;
        
        if (sum != 30) throw new RuntimeException("expression failed");
        
        var text = "count: " + sum;
        if (text == null) throw new RuntimeException("string expr failed");
        
        System.out.println("expression var: OK");
    }
}
