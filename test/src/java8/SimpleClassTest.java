/**
 * Simple test with multiple classes in one file.
 */

class Helper {
    public String greet() {
        return "Hello!";
    }
}

public class SimpleClassTest {
    public static void main(String[] args) {
        Helper h = new Helper();
        String msg = h.greet();
        System.out.println(msg);
        System.out.println("SimpleClassTest passed!");
    }
}

