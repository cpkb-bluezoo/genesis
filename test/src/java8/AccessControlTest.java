/**
 * Test access control (public/private/protected/package-private).
 */

class Helper {
    public int publicField = 1;
    protected int protectedField = 2;
    int packageField = 3;  // package-private (default)
    private int privateField = 4;
    
    public int getPublic() { return publicField; }
    protected int getProtected() { return protectedField; }
    int getPackage() { return packageField; }
    private int getPrivate() { return privateField; }
    
    // Method to test private access from within same class
    public int testSelfAccess() {
        return privateField + getPrivate();
    }
}

public class AccessControlTest {
    public static void main(String[] args) {
        Helper h = new Helper();
        
        // Public access should work
        System.out.println("publicField = " + h.publicField);
        System.out.println("getPublic() = " + h.getPublic());
        
        // Protected access should work (same package)
        System.out.println("protectedField = " + h.protectedField);
        System.out.println("getProtected() = " + h.getProtected());
        
        // Package-private access should work (same package)
        System.out.println("packageField = " + h.packageField);
        System.out.println("getPackage() = " + h.getPackage());
        
        // Test that private access works from within the class
        System.out.println("testSelfAccess() = " + h.testSelfAccess());
        
        System.out.println("AccessControlTest passed!");
    }
}

