/**
 * Test for captured variables in local and anonymous classes.
 */
public class CaptureTest {
    
    int outerField = 10;
    
    interface IntGetter {
        int get();
    }
    
    /**
     * Test 1: Local class captures a local variable
     */
    public void testLocalCapture() {
        int localValue = 42;
        
        class Helper {
            int getLocal() {
                return localValue;  // Captures localValue
            }
        }
        
        Helper h = new Helper();
        int result = h.getLocal();
        if (result != 42) {
            throw new RuntimeException("testLocalCapture failed");
        }
        System.out.println("testLocalCapture passed");
    }
    
    /**
     * Test 2: Anonymous class captures a local variable
     */
    public void testAnonymousCapture() {
        int localValue = 99;
        
        IntGetter g = new IntGetter() {
            public int get() {
                return localValue;  // Captures localValue
            }
        };
        
        int result = g.get();
        if (result != 99) {
            throw new RuntimeException("testAnonymousCapture failed");
        }
        System.out.println("testAnonymousCapture passed");
    }
    
    /**
     * Test 3: Anonymous class captures multiple variables
     */
    public void testMultipleCapture() {
        int a = 10;
        int b = 20;
        
        IntGetter g = new IntGetter() {
            public int get() {
                return a + b;  // Captures both a and b
            }
        };
        
        int result = g.get();
        if (result != 30) {
            throw new RuntimeException("testMultipleCapture failed");
        }
        System.out.println("testMultipleCapture passed");
    }
    
    /**
     * Test 4: Anonymous class captures variable AND accesses outer field
     */
    public void testCaptureAndOuter() {
        int localValue = 5;
        
        IntGetter g = new IntGetter() {
            public int get() {
                return localValue + outerField;  // Captures local + access outer
            }
        };
        
        int result = g.get();
        if (result != 15) {  // 5 + 10
            throw new RuntimeException("testCaptureAndOuter failed");
        }
        System.out.println("testCaptureAndOuter passed");
    }
    
    public static void main(String[] args) {
        CaptureTest test = new CaptureTest();
        test.testLocalCapture();
        test.testAnonymousCapture();
        test.testMultipleCapture();
        test.testCaptureAndOuter();
        System.out.println("CaptureTest passed!");
    }
}

