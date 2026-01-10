/**
 * Test wildcard types in generics - basic parsing and type checking.
 */
public class WildcardTest {
    
    // Methods with wildcard parameters
    public static void unboundedWildcard(Class<?> clazz) {
        System.out.println("Class: " + clazz.getName());
    }
    
    public static void extendsWildcard(Class<? extends Number> clazz) {
        System.out.println("Number subclass: " + clazz.getName());
    }
    
    public static void superWildcard(Class<? super Integer> clazz) {
        System.out.println("Integer superclass: " + clazz.getName());
    }
    
    public static void main(String[] args) {
        // Test unbounded wildcard
        WildcardTest.unboundedWildcard(String.class);
        WildcardTest.unboundedWildcard(Integer.class);
        
        // Test extends wildcard
        WildcardTest.extendsWildcard(Integer.class);
        WildcardTest.extendsWildcard(Double.class);
        
        // Test super wildcard  
        WildcardTest.superWildcard(Integer.class);
        WildcardTest.superWildcard(Number.class);
        
        System.out.println("WildcardTest passed!");
    }
}
