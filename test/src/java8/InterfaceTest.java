/**
 * Test interface declaration and implementation.
 */

interface Greeting {
    String getMessage();
}

interface Printable {
    void print();
}

class HelloGreeting implements Greeting {
    public String getMessage() {
        return "Hello!";
    }
}

class MultiImpl implements Greeting, Printable {
    public String getMessage() {
        return "Multi!";
    }
    
    public void print() {
        System.out.println("Printing from MultiImpl");
    }
}

public class InterfaceTest {
    public static void main(String[] args) {
        // Test basic interface implementation
        HelloGreeting hello = new HelloGreeting();
        String msg = hello.getMessage();
        System.out.println("HelloGreeting: " + msg);
        
        // Test multiple interface implementation
        MultiImpl multi = new MultiImpl();
        multi.print();
        
        // Test interface reference
        Greeting g = new HelloGreeting();
        System.out.println("Via interface: " + g.getMessage());
        
        // Test polymorphism with interface
        Printable p = new MultiImpl();
        p.print();
        
        System.out.println("InterfaceTest passed!");
    }
}

