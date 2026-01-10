public class RecordPatternTest {
    
    public static void main(String[] args) {
        testCase1();
        testCase2();
        testCase3();
        System.out.println("PASS: All record pattern tests passed");
    }
    
    static void testCase1() {
        Object obj = new TestPoint(5, 12);
        int distance = switch (obj) {
            case TestPoint(int x, int y) -> x * x + y * y;
            default -> 0;
        };
        System.out.println("Test 1: " + distance);
        if (distance != 169) {
            System.out.println("FAIL: expected 169");
            System.exit(1);
        }
    }
    
    static void testCase2() {
        Object obj = new TestPoint(3, 4);
        String result = switch (obj) {
            case TestPoint(int x, int y) -> "matched";
            default -> "not matched";
        };
        System.out.println("Test 2: " + result);
        if (!result.equals("matched")) {
            System.out.println("FAIL: expected 'matched'");
            System.exit(1);
        }
    }
    
    static void testCase3() {
        Object obj = "hello";
        String result = switch (obj) {
            case TestPoint(int x, int y) -> "is point";
            default -> "not point";
        };
        System.out.println("Test 3: " + result);
        if (!result.equals("not point")) {
            System.out.println("FAIL: expected 'not point'");
            System.exit(1);
        }
    }
}
