import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.Files;

/**
 * Test java.nio.file imports are resolved correctly.
 */
public class NioPathTest {
    public static void main(String[] args) throws Exception {
        // Test Paths.get
        Path p = Paths.get("/tmp");
        System.out.println("Path: " + p);
        
        // Test Files static method
        boolean exists = Files.exists(p);
        System.out.println("Exists: " + exists);
        
        System.out.println("NioPathTest passed!");
    }
}

