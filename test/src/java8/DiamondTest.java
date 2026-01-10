/*
 * Test for diamond operator (Java 7)
 * Tests type inference with <>
 */
public class DiamondTest {
    
    /* Simple generic holder class */
    static class Box<T> {
        T value;
        
        public Box() {
            this.value = null;
        }
        
        public void set(T v) {
            this.value = v;
        }
        
        public T get() {
            return value;
        }
    }
    
    public static void main(String[] args) {
        /* Test 1: Diamond with no-arg constructor */
        Box<String> box1 = new Box<>();
        box1.set("hello");
        
        Object val1 = box1.get();
        if (val1 == null) {
            System.out.println("FAIL: box1 value is null");
            return;
        }
        
        /* Test 2: Another diamond usage */
        Box<Object> box2 = new Box<>();
        box2.set("world");
        
        Object val2 = box2.get();
        if (val2 == null) {
            System.out.println("FAIL: box2 value is null");
            return;
        }
        
        System.out.println("PASS");
    }
}

