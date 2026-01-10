/**
 * Test for Java 16 record feature.
 */
public class RecordTest {
    
    // Simple record with two components
    public record Point(int x, int y) {}
    
    // Record with reference type
    public record Person(String name, int age) {}
    
    public static void main(String[] args) {
        // Test Point record - accessor methods
        Point p1 = new Point(10, 20);
        Point p2 = new Point(10, 20);
        Point p3 = new Point(5, 15);
        
        System.out.println("Testing Point record:");
        int xval = p1.x();
        int yval = p1.y();
        System.out.println("p1.x() = " + xval);
        System.out.println("p1.y() = " + yval);
        
        // Test toString
        String str = p1.toString();
        System.out.println("p1.toString() = " + str);
        
        // Test hashCode
        int h1 = p1.hashCode();
        int h2 = p2.hashCode();
        boolean hashEq = (h1 == h2);
        System.out.println("p1.hashCode() == p2.hashCode() = " + hashEq);
        
        // Test Person record with String component
        System.out.println("\nTesting Person record:");
        Person alice = new Person("Alice", 30);
        
        System.out.println("alice.name() = " + alice.name());
        System.out.println("alice.age() = " + alice.age());
        System.out.println("alice.toString() = " + alice.toString());
        
        System.out.println("\nAll record tests passed!");
    }
}

