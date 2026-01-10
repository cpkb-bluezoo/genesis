/**
 * Test for try-with-resources (Java 7).
 * Uses a simple custom AutoCloseable for testing.
 */
public class TryWithResourcesTest {
    
    static class SimpleResource implements AutoCloseable {
        private int value;
        boolean closed = false;
        
        SimpleResource(int v) {
            this.value = v;
        }
        
        int getValue() {
            return value;
        }
        
        boolean isClosed() {
            return closed;
        }
        
        public void close() {
            closed = true;
        }
    }
    
    public static void main(String[] args) {
        // Test: basic TWR - resource should be closed after try block
        SimpleResource resource = null;
        try (SimpleResource r = new SimpleResource(42)) {
            resource = r;
            int v = r.getValue();
            if (v != 42) {
                System.out.println("FAIL: wrong value");
                return;
            }
        }
        if (resource == null) {
            System.out.println("FAIL: resource is null");
            return;
        }
        if (!resource.isClosed()) {
            System.out.println("FAIL: resource not closed");
            return;
        }
        
        System.out.println("PASS");
    }
}

