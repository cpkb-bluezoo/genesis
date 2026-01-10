package test;

public class Simple {
    
    private int value;
    private String name;
    
    public Simple(int v) {
        value = v;
    }
    
    public int getValue() {
        return value;
    }
    
    public void setValue(int v) {
        value = v;
    }
    
    public int add(int a, int b) {
        int result = a + b;
        return result;
    }
    
    public boolean isPositive() {
        return value > 0;
    }
}

