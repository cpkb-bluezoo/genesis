/**
 * Test enum with simple custom fields and methods (no constructor params).
 */
public class EnumSimpleFieldTest {
    
    public enum Status {
        ACTIVE, INACTIVE, PENDING;
        
        // Custom field
        private int code;
        
        // Custom method
        public int getCode() {
            return code;
        }
        
        public void setCode(int c) {
            code = c;
        }
    }
    
    public static void main(String[] args) {
        Status s = Status.ACTIVE;
        s.setCode(100);
        int code = s.getCode();
        System.out.println("Status code: " + code);
        
        if (code != 100) {
            throw new RuntimeException("getCode returned wrong value: " + code);
        }
        
        System.out.println("EnumSimpleFieldTest passed!");
    }
}

