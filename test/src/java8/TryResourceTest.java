import java.io.*;

// Custom AutoCloseable for testing
class SimpleResource implements AutoCloseable {
    private final String name;
    private boolean closed = false;
    
    public SimpleResource(String name) {
        this.name = name;
        System.out.println("Opened: " + name);
    }
    
    public void doWork() {
        System.out.println("Working with: " + name);
    }
    
    public void close() {
        closed = true;
        System.out.println("Closed: " + name);
    }
    
    public boolean isClosed() {
        return closed;
    }
}

public class TryResourceTest {
    
    // Test Java 9 try-with-resources on existing variable
    public static void testExistingVariable() {
        SimpleResource res = new SimpleResource("Resource1");
        
        // Java 9+: Use existing effectively final variable
        try (res) {
            res.doWork();
        }
        
        System.out.println("Closed after try: " + res.isClosed());
    }
    
    // Test traditional try-with-resources (declaration form)
    public static void testDeclarationForm() {
        try (SimpleResource res = new SimpleResource("Resource2")) {
            res.doWork();
        }
    }
    
    // Test with multiple resources (mixed forms)
    public static void testMixedForms() {
        SimpleResource first = new SimpleResource("First");
        
        try (first; SimpleResource second = new SimpleResource("Second")) {
            first.doWork();
            second.doWork();
        }
        
        System.out.println("First closed: " + first.isClosed());
    }
    
    public static void main(String[] args) {
        testExistingVariable();
        System.out.println("---");
        testDeclarationForm();
        System.out.println("---");
        testMixedForms();
        System.out.println("---");
        System.out.println("All tests passed!");
    }
}
