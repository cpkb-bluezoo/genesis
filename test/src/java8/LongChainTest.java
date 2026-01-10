public class LongChainTest {
    
    public int value;
    public LongChainTest next;
    
    public LongChainTest() {
        this.value = 0;
        this.next = null;
    }
    
    public void setNext(LongChainTest n) {
        this.next = n;
    }
    
    public LongChainTest getNext() {
        return this.next;
    }
    
    public void setValue(int v) {
        this.value = v;
    }
    
    public int getValue() {
        return this.value;
    }
    
    public static void main(String[] args) {
        System.out.println("Testing long chains...");
        
        // Create chain: node1 -> node2 -> node3
        LongChainTest node1 = new LongChainTest();
        LongChainTest node2 = new LongChainTest();
        LongChainTest node3 = new LongChainTest();
        
        node1.setValue(1);
        node2.setValue(2);
        node3.setValue(3);
        
        node1.setNext(node2);
        node2.setNext(node3);
        
        // Test single-level access
        System.out.println("node1.value:");
        System.out.println(node1.getValue());
        
        // Test two-level access via method call
        System.out.println("node1.next via getNext():");
        LongChainTest n2 = node1.getNext();
        System.out.println(n2.getValue());
        
        // Test field access on returned object
        System.out.println("node1.getNext().getValue():");
        System.out.println(node1.getNext().getValue());
        
        // Test deeper chain
        System.out.println("node1.getNext().getNext().getValue():");
        System.out.println(node1.getNext().getNext().getValue());
        
        System.out.println("All chain tests passed!");
    }
}
