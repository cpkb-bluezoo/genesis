// Constructor reference tests
public class ConstructorRefTest {
    
    // Simple value holder class with a constructor
    static class Box {
        private String value;
        
        public Box() {
            this.value = "empty";
        }
        
        public String getValue() {
            return value;
        }
    }
    
    @FunctionalInterface
    interface BoxFactory {
        Box create();
    }
    
    public static void main(String[] args) {
        // Test: Constructor reference on local class with no-arg constructor
        BoxFactory bf = Box::new;
        Box b = bf.create();
        System.out.println("Box value: " + b.getValue());
        
        System.out.println("Constructor reference test completed!");
    }
}

