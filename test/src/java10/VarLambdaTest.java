public class VarLambdaTest {
    
    @FunctionalInterface
    interface IntBinaryOp {
        int apply(int a, int b);
    }
    
    public static void main(String[] args) {
        // Test var in lambda parameters (Java 11)
        IntBinaryOp add = (var x, var y) -> x + y;
        int result = add.apply(3, 5);
        System.out.println("add(3, 5) = " + result);
        
        if (result == 8) {
            System.out.println("Test passed!");
        } else {
            System.out.println("Test failed!");
        }
    }
}
