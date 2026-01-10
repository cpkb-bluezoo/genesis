// Array constructor reference test
public class ArrayConstructorRefTest {
    
    @FunctionalInterface
    interface ArrayFactory {
        String[] create(int size);
    }
    
    public static void main(String[] args) {
        // Array constructor reference
        ArrayFactory factory = String[]::new;
        String[] arr = factory.create(3);
        System.out.println("Array length: " + arr.length);
        System.out.println("Array constructor reference test completed!");
    }
}

