/**
 * Test static nested classes.
 * Static nested classes can access static members of the enclosing class.
 * They generate separate class files with OuterClass$NestedClass naming.
 */
public class StaticNestedTest {
    static int outerStatic = 42;  // package-private, accessible by nested class
    static String outerString = "Hello";
    
    public static class Nested {
        private int nestedValue = 10;
        
        public int getValue() {
            return outerStatic;  // Access outer static field
        }
        
        public String getString() {
            return outerString;
        }
        
        public int getNestedValue() {
            return nestedValue;
        }
    }
    
    public static class Inner2 {
        public int doubleOuter() {
            return outerStatic * 2;
        }
    }
    
    public static void main(String[] args) {
        // Test basic nested class
        Nested n = new Nested();
        System.out.println("Nested value: " + n.getValue());
        System.out.println("Nested string: " + n.getString());
        System.out.println("Nested own value: " + n.getNestedValue());
        
        // Test second nested class
        Inner2 i2 = new Inner2();
        System.out.println("Double outer: " + i2.doubleOuter());
        
        System.out.println("StaticNestedTest passed!");
    }
}
