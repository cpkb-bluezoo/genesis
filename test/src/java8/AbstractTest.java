/**
 * Test abstract classes and methods.
 */

abstract class Shape {
    protected String name;
    
    public Shape(String name) {
        this.name = name;
    }
    
    // Abstract method - must be implemented by subclasses
    public abstract int getSize();
    
    // Concrete method
    public String getName() {
        return name;
    }
}

class Square extends Shape {
    private int side;
    
    public Square(int side) {
        super("Square");
        this.side = side;
    }
    
    public int getSize() {
        return side * side;
    }
}

public class AbstractTest {
    public static void main(String[] args) {
        // Test Square
        Square sq = new Square(5);
        System.out.println("Square size: " + sq.getSize());
        System.out.println("Name: " + sq.getName());
        
        // Test polymorphism with abstract class reference
        Shape s = new Square(3);
        System.out.println("Shape size: " + s.getSize());
        
        System.out.println("AbstractTest passed!");
    }
}
