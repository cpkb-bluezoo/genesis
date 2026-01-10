public class NegatedMethodTest {
    public static void main(String[] args) {
        String name = "Inner";
        
        // Test negated method call
        if (!name.equals("Outer")) {
            System.out.println("name is not Outer");
        }
        
        System.out.println("NegatedMethodTest passed!");
    }
}
