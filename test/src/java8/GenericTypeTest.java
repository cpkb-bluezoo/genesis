/**
 * Test generic type parameters and type erasure.
 * Tests basic generic class support with type parameters.
 */
public class GenericTypeTest<T> {
    private T value;
    
    public T getValue() {
        return value;
    }
    
    public void setValue(T v) {
        value = v;
    }
    
    public static void main(String[] args) {
        // Test basic generic class instantiation with String
        GenericTypeTest<String> stringBox = new GenericTypeTest<String>();
        stringBox.setValue("Hello Generics");
        Object result = stringBox.getValue();
        if (result == null) {
            throw new RuntimeException("getValue returned null");
        }
        
        // Test assignment works correctly (type erasure to Object)
        String greeting = (String) result;
        System.out.println(greeting);
        
        System.out.println("GenericTypeTest passed!");
    }
}
