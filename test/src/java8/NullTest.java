public class NullTest {
    
    public NullTest() {
    }
    
    public static void main(String[] args) {
        System.out.println("Testing null support...");
        
        // Test 1: Assign null to reference
        String s = null;
        System.out.println("Assigned null to String");
        
        // Test 2: Check if null
        if (s == null) {
            System.out.println("s is null");
        }
        
        // Test 3: Assign value and check not null
        s = "Hello";
        if (s != null) {
            System.out.println("s is not null: " + s);
        }
        
        // Test 4: Null in array
        String[] arr = new String[3];
        arr[0] = "First";
        arr[1] = null;
        arr[2] = "Third";
        
        if (arr[1] == null) {
            System.out.println("arr[1] is null");
        }
        
        // Test 5: Return null from method
        String result = getNullString();
        if (result == null) {
            System.out.println("Method returned null");
        }
        
        System.out.println("All null tests passed!");
    }
    
    public static String getNullString() {
        return null;
    }
}

