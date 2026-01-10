public class CtorChainTest {
    
    private int value;
    
    // Default constructor chains to parameterized constructor
    public CtorChainTest() {
        this(42);
        System.out.println("Default constructor - value is: " + value);
    }
    
    // Parameterized constructor
    public CtorChainTest(int value) {
        super();  // Explicit super call
        this.value = value;
        System.out.println("Parameterized constructor - set value to: " + value);
    }
    
    public int getValue() {
        return value;
    }
    
    public static void main(String[] args) {
        System.out.println("Testing constructor chaining...");
        
        System.out.println("\n1. Creating with default constructor:");
        CtorChainTest obj1 = new CtorChainTest();
        int v1 = obj1.getValue();
        System.out.println("obj1.getValue() = " + v1);
        
        System.out.println("\n2. Creating with parameterized constructor:");
        CtorChainTest obj2 = new CtorChainTest(100);
        int v2 = obj2.getValue();
        System.out.println("obj2.getValue() = " + v2);
        
        System.out.println("\nAll constructor chaining tests passed!");
    }
}
