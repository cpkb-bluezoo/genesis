public class DeepChainTest {
    
    public int value;
    
    public DeepChainTest() {
        this.value = 0;
    }
    
    public void setValue(int v) {
        this.value = v;
    }
    
    public int getValue() {
        return this.value;
    }
    
    public static void main(String[] args) {
        System.out.println("Testing this.field access...");
        
        DeepChainTest obj = new DeepChainTest();
        obj.setValue(42);
        int result = obj.getValue();
        System.out.println(result);
        
        System.out.println("Done!");
    }
}
