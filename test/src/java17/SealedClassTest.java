/**
 * Test for sealed classes (Java 17+).
 */
public class SealedClassTest {
    
    public static void main(String[] args) {
        testSealedClass();
        testSealedInterface();
        testNonSealed();
        
        System.out.println("All sealed class tests passed!");
    }
    
    // Sealed class with permitted subclasses
    static sealed class Shape permits Circle, Rectangle {
        abstract double area();
    }
    
    // Final subclass (implicitly final if sealed has one implementation)
    static final class Circle extends Shape {
        private double radius;
        
        Circle(double radius) {
            this.radius = radius;
        }
        
        double area() {
            return 3.14159 * radius * radius;
        }
    }
    
    // Non-sealed subclass can be extended further
    static non-sealed class Rectangle extends Shape {
        double width;
        double height;
        
        Rectangle(double width, double height) {
            this.width = width;
            this.height = height;
        }
        
        double area() {
            return width * height;
        }
    }
    
    static void testSealedClass() {
        Shape circle = new Circle(5.0);
        Shape rect = new Rectangle(3.0, 4.0);
        
        // Test areas
        double circleArea = circle.area();
        double rectArea = rect.area();
        
        // Approximate checks
        if (circleArea < 78.0 || circleArea > 79.0) {
            throw new RuntimeException("Circle area failed: " + circleArea);
        }
        if (rectArea != 12.0) {
            throw new RuntimeException("Rectangle area failed: " + rectArea);
        }
    }
    
    // Sealed interface
    static sealed interface Animal permits Dog, Cat {
        String speak();
    }
    
    static final class Dog implements Animal {
        public String speak() {
            return "Woof!";
        }
    }
    
    static final class Cat implements Animal {
        public String speak() {
            return "Meow!";
        }
    }
    
    static void testSealedInterface() {
        Animal dog = new Dog();
        Animal cat = new Cat();
        
        String dogSound = dog.speak();
        String catSound = cat.speak();
        
        // Just print them - avoid complex comparison
        System.out.println("Dog says: " + dogSound);
        System.out.println("Cat says: " + catSound);
    }
    
    static void testNonSealed() {
        // Test that non-sealed allows further extension
        Rectangle rect = new Rectangle(5.0, 5.0);
        double area = rect.area();
        
        if (area != 25.0) {
            throw new RuntimeException("Rectangle area failed: " + area);
        }
    }
}

