/**
 * Helper class for testing sourcepath import resolution.
 * This class is in a separate file and must be found via -sourcepath.
 */
public class SourcepathHelper {
    private int value;
    
    public SourcepathHelper(int value) {
        this.value = value;
    }
    
    public static String getStaticMessage() {
        return "Hello from SourcepathHelper!";
    }
    
    public static int add(int a, int b) {
        return a + b;
    }
    
    public int getValue() {
        return value;
    }
    
    public void setValue(int value) {
        this.value = value;
    }
}

