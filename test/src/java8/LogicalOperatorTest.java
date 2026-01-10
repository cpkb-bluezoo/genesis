/**
 * Test logical AND (&&) and OR (||) operators.
 */
public class LogicalOperatorTest {
    
    private static int andCallCount = 0;
    private static int orCallCount = 0;
    
    public static void main(String[] args) {
        // Test basic && behavior
        testBasicAnd();
        
        // Test basic || behavior
        testBasicOr();
        
        // Test short-circuit evaluation for &&
        testShortCircuitAnd();
        
        // Test short-circuit evaluation for ||
        testShortCircuitOr();
        
        // Test combined expressions
        testCombined();
        
        System.out.println("LogicalOperatorTest passed!");
    }
    
    private static void testBasicAnd() {
        // true && true = true
        if (trueValue() && trueValue()) {
            System.out.println("&& true/true: OK");
        } else {
            throw new RuntimeException("FAILED: true && true should be true");
        }
        
        // true && false = false
        if (trueValue() && falseValue()) {
            throw new RuntimeException("FAILED: true && false should be false");
        } else {
            System.out.println("&& true/false: OK");
        }
        
        // false && true = false
        if (falseValue() && trueValue()) {
            throw new RuntimeException("FAILED: false && true should be false");
        } else {
            System.out.println("&& false/true: OK");
        }
        
        // false && false = false
        if (falseValue() && falseValue()) {
            throw new RuntimeException("FAILED: false && false should be false");
        } else {
            System.out.println("&& false/false: OK");
        }
    }
    
    private static void testBasicOr() {
        // true || true = true
        if (trueValue() || trueValue()) {
            System.out.println("|| true/true: OK");
        } else {
            throw new RuntimeException("FAILED: true || true should be true");
        }
        
        // true || false = true
        if (trueValue() || falseValue()) {
            System.out.println("|| true/false: OK");
        } else {
            throw new RuntimeException("FAILED: true || false should be true");
        }
        
        // false || true = true
        if (falseValue() || trueValue()) {
            System.out.println("|| false/true: OK");
        } else {
            throw new RuntimeException("FAILED: false || true should be true");
        }
        
        // false || false = false
        if (falseValue() || falseValue()) {
            throw new RuntimeException("FAILED: false || false should be false");
        } else {
            System.out.println("|| false/false: OK");
        }
    }
    
    private static void testShortCircuitAnd() {
        // Reset counters
        andCallCount = 0;
        
        // false && <anything> should not evaluate right side
        if (falseValue() && incrementAnd()) {
            throw new RuntimeException("FAILED: false && x should be false");
        }
        
        if (andCallCount != 0) {
            throw new RuntimeException("FAILED: && should short-circuit when left is false");
        }
        System.out.println("&& short-circuit: OK");
    }
    
    private static void testShortCircuitOr() {
        // Reset counters
        orCallCount = 0;
        
        // true || <anything> should not evaluate right side
        if (trueValue() || incrementOr()) {
            // Expected
        } else {
            throw new RuntimeException("FAILED: true || x should be true");
        }
        
        if (orCallCount != 0) {
            throw new RuntimeException("FAILED: || should short-circuit when left is true");
        }
        System.out.println("|| short-circuit: OK");
    }
    
    private static void testCombined() {
        // Test combined with comparison
        int x = 5;
        int y = 10;
        
        if (x > 0 && y > 0) {
            System.out.println("Combined with comparison: OK");
        } else {
            throw new RuntimeException("FAILED: 5 > 0 && 10 > 0 should be true");
        }
        
        if (x > 100 || y > 5) {
            System.out.println("Combined || with comparison: OK");
        } else {
            throw new RuntimeException("FAILED: 5 > 100 || 10 > 5 should be true");
        }
    }
    
    private static boolean trueValue() {
        return true;
    }
    
    private static boolean falseValue() {
        return false;
    }
    
    private static boolean incrementAnd() {
        andCallCount = andCallCount + 1;
        return true;
    }
    
    private static boolean incrementOr() {
        orCallCount = orCallCount + 1;
        return false;
    }
}

