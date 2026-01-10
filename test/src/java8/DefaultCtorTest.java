// Test class with no explicit constructor
public class DefaultCtorTest {
    
    private int value;
    
    // No constructor defined - compiler should generate default
    
    public void setValue(int value) {
        this.value = value;
    }
    
    public int getValue() {
        return value;
    }
    
    public static void main(String[] args) {
        System.out.println("Testing default constructor generation...");
        
        // This requires a default no-arg constructor
        DefaultCtorTest obj = new DefaultCtorTest();
        
        System.out.println("Object created successfully");
        
        obj.setValue(42);
        int v = obj.getValue();
        System.out.println("Value after setValue(42): " + v);
        
        // Create another instance
        DefaultCtorTest obj2 = new DefaultCtorTest();
        System.out.println("Second object created");
        
        System.out.println("All default constructor tests passed!");
    }
}

