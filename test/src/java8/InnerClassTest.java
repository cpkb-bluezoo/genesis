/**
 * Test inner class (non-static nested class) support.
 * Inner classes have an implicit reference to the enclosing instance.
 */
public class InnerClassTest {
    
    // Note: Using package-private access because we haven't implemented
    // synthetic accessor methods for private field access from inner classes
    int outerValue = 10;
    String outerName = "Outer";
    
    /**
     * Simple inner class that accesses outer instance fields
     */
    public class Inner {
        int innerValue = 20;
        
        public int getInnerValue() {
            return innerValue;
        }
        
        public int getOuterValue() {
            // Access enclosing instance's field via implicit this$0
            return outerValue;
        }
        
        public int getSum() {
            return outerValue + innerValue;
        }
        
        public String getOuterName() {
            return outerName;
        }
    }
    
    /**
     * Inner class with constructor parameters
     */
    public class InnerWithParam {
        int value;
        
        public InnerWithParam(int val) {
            this.value = val;
        }
        
        public int getValue() {
            return value;
        }
        
        public int getCombined() {
            return outerValue + value;
        }
    }
    
    public Inner createInner() {
        return new Inner();
    }
    
    public InnerWithParam createInnerWithParam(int val) {
        return new InnerWithParam(val);
    }
    
    public void setOuterValue(int val) {
        outerValue = val;
    }
    
    public void runTests() {
        // Create inner class instance - 'new Inner()' works inside the outer class
        Inner inner = new Inner();
        
        System.out.println("Inner value: " + inner.getInnerValue());
        System.out.println("Outer value via inner: " + inner.getOuterValue());
        System.out.println("Sum: " + inner.getSum());
        System.out.println("Outer name: " + inner.getOuterName());
        
        // Create another inner via factory method
        Inner inner2 = createInner();
        System.out.println("Inner2 sum: " + inner2.getSum());
        
        // Test inner class with constructor param
        InnerWithParam inner3 = new InnerWithParam(5);
        System.out.println("InnerWithParam value: " + inner3.getValue());
        System.out.println("Combined: " + inner3.getCombined());
        
        // Modify outer and see inner reflects it
        setOuterValue(100);
        System.out.println("After outer change, inner gets: " + inner.getOuterValue());
        System.out.println("After outer change, sum: " + inner.getSum());
        
        System.out.println("InnerClassTest passed!");
    }
    
    public static void main(String[] args) {
        InnerClassTest outer = new InnerClassTest();
        outer.runTests();
    }
}

