public class SuperCallTest {
    private int value;
    
    public SuperCallTest() {
        super();  // Explicit call to Object()
        this.value = 0;
        System.out.println("SuperCallTest() - value set to 0");
    }
    
    public SuperCallTest(int value) {
        super();  // Explicit call to Object()
        this.value = value;
        System.out.println("SuperCallTest(int) - value set");
    }
    
    public int getValue() {
        return value;
    }
    
    public static void main(String[] args) {
        System.out.println("Testing super() calls...\n");
        
        System.out.println("1. Default constructor:");
        SuperCallTest obj1 = new SuperCallTest();
        int v1 = obj1.getValue();
        System.out.println("  value=" + v1);
        
        System.out.println("\n2. Parameterized constructor:");
        SuperCallTest obj2 = new SuperCallTest(42);
        int v2 = obj2.getValue();
        System.out.println("  value=" + v2);
        
        System.out.println("\nAll super() call tests passed!");
    }
}
