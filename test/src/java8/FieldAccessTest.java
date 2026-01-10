class SimpleClass {
    int value;
    long bigValue;
    String name;
    
    public SimpleClass() {
        value = 42;
        bigValue = 1234567890L;
        name = "test";
    }
}

public class FieldAccessTest {
    
    public static void main(String[] args) {
        System.out.println("Testing field access on local variables...");
        
        SimpleClass obj = new SimpleClass();
        
        // Access int field
        int v = obj.value;
        System.out.println("obj.value = " + v);
        
        // Access long field
        long bv = obj.bigValue;
        System.out.println("obj.bigValue = " + bv);
        
        // Access String field
        String n = obj.name;
        System.out.println("obj.name = " + n);
        
        System.out.println("All field access tests passed!");
    }
}
