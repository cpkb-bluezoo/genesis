public class ParamTest {
    
    private int value;
    
    // Constructor with different parameter name than field
    public ParamTest(int x) {
        this.value = x;
    }
    
    public int getValue() {
        return value;
    }
    
    public static void main(String[] args) {
        System.out.println("Testing constructor parameters...");
        
        ParamTest obj = new ParamTest(42);
        int v = obj.getValue();
        System.out.println("value = " + v);
        
        System.out.println("Done!");
    }
}

