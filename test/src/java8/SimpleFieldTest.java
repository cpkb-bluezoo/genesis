public class SimpleFieldTest {
    
    public int value;
    
    public void setValue(int v) {
        this.value = v;
    }
    
    public int getValue() {
        return this.value;
    }
    
    public static void main(String[] args) {
        System.out.println("Testing field access...");
        System.out.println(123);
        System.out.println("Done!");
    }
}

