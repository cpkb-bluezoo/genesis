public class SynchronizedTest {
    
    private int counter;
    private Object lock;
    
    public SynchronizedTest() {
        this.counter = 0;
        this.lock = new Object();
    }
    
    public void incrementWithLock() {
        synchronized(this.lock) {
            this.counter = this.counter + 1;
        }
    }
    
    public void incrementWithThis() {
        synchronized(this) {
            this.counter = this.counter + 1;
        }
    }
    
    public int getCounter() {
        return this.counter;
    }
    
    public static void main(String[] args) {
        System.out.println("Testing synchronized blocks...");
        
        SynchronizedTest test = new SynchronizedTest();
        
        // Test 1: synchronized with explicit lock object
        System.out.println("Test 1: synchronized(lock)");
        test.incrementWithLock();
        System.out.println(test.getCounter());
        
        // Test 2: synchronized with 'this'
        System.out.println("Test 2: synchronized(this)");
        test.incrementWithThis();
        System.out.println(test.getCounter());
        
        // Test 3: Multiple synchronized calls
        System.out.println("Test 3: Multiple calls");
        test.incrementWithLock();
        test.incrementWithLock();
        test.incrementWithThis();
        System.out.println(test.getCounter());
        
        System.out.println("All synchronized tests passed!");
    }
}
