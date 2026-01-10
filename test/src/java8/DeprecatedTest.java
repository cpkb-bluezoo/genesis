// Test that @Deprecated annotation is written to class file
@Deprecated
public class DeprecatedTest {
    
    @Deprecated
    public void oldMethod() {
        System.out.println("oldMethod called");
    }
    
    public void newMethod() {
        System.out.println("newMethod called");
    }
    
    public static void main(String[] args) {
        DeprecatedTest test = new DeprecatedTest();
        test.oldMethod();
        test.newMethod();
        System.out.println("DeprecatedTest passed!");
    }
}


