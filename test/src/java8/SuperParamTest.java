class ParamParent {
    int parentValue;
    
    public ParamParent(int v) {
        parentValue = v;
        System.out.println("Parent(int) called with: " + v);
    }
}

public class SuperParamTest extends ParamParent {
    int childValue;
    
    // Constructor that passes parameter to super()
    public SuperParamTest(int x) {
        super(x);  // Pass x to parent - this worked after fix!
        childValue = x;
        System.out.println("Child(int) called with: " + x);
    }
    
    public static void main(String[] args) {
        System.out.println("Testing parameter passing to super...");
        
        SuperParamTest obj = new SuperParamTest(42);
        System.out.println("childValue = " + obj.childValue);
        
        System.out.println("Done!");
    }
}

