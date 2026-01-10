public class OverloadTest {
    
    public static void main(String[] args) {
        System.out.println("Testing method overload resolution...");
        
        // Test 1: Print int array element
        int[] intArr = {10, 20, 30};
        System.out.println("intArr[0] = ");
        System.out.println(intArr[0]);  // Should call println(int)
        
        // Test 2: Print long array element
        long[] longArr = {100L, 200L, 300L};
        System.out.println("longArr[1] = ");
        System.out.println(longArr[1]);  // Should call println(long)
        
        // Test 3: Print String array element
        String[] strArr = {"Hello", "World"};
        System.out.println("strArr[0] = ");
        System.out.println(strArr[0]);  // Should call println(String)
        
        // Test 4: Print double array element
        double[] doubleArr = {1.5, 2.5, 3.5};
        System.out.println("doubleArr[2] = ");
        System.out.println(doubleArr[2]);  // Should call println(double)
        
        System.out.println("All overload tests passed!");
    }
}

