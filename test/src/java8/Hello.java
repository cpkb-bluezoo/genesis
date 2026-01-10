package com.example;

/**
 * A simple class for testing the genesis compiler.
 * This is a support class, not a test (no main method).
 */
public class Hello {
    
    private String message;
    
    public Hello(String message) {
        this.message = message;
    }
    
    public void sayHello() {
        System.out.println("Hello, " + message + "!");
    }
    
    public String getMessage() {
        return message;
    }
}
