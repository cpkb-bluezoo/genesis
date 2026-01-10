public class SimpleCatchTest {
    public static void main(String[] args) {
        try {
            throw new RuntimeException("test");
        } catch (RuntimeException ex) {
            RuntimeException rex = ex;  // Simple assignment
            System.out.println("Caught exception");
        }
        System.out.println("SimpleCatchTest passed!");
    }
}
