/**
 * Test for strings in switch statements (Java 7).
 */
public class StringSwitchTest {
    
    static String testSwitch(String s) {
        switch (s) {
            case "hello":
                return "greeting";
            case "goodbye":
                return "farewell";
            case "yes":
                return "affirmative";
            case "no":
                return "negative";
            default:
                return "unknown";
        }
    }
    
    static int testWithFallthrough(String day) {
        int result;
        switch (day) {
            case "Monday":
            case "Tuesday":
            case "Wednesday":
            case "Thursday":
            case "Friday":
                result = 1;  // Weekday
                break;
            case "Saturday":
            case "Sunday":
                result = 2;  // Weekend
                break;
            default:
                result = 0;
                break;
        }
        return result;
    }
    
    public static void main(String[] args) {
        // Test basic string switch
        String result = testSwitch("hello");
        if (result == null || !result.equals("greeting")) {
            System.out.println("FAIL: hello should return greeting, got " + result);
            return;
        }
        result = testSwitch("goodbye");
        if (result == null || !result.equals("farewell")) {
            System.out.println("FAIL: goodbye should return farewell, got " + result);
            return;
        }
        result = testSwitch("yes");
        if (result == null || !result.equals("affirmative")) {
            System.out.println("FAIL: yes should return affirmative, got " + result);
            return;
        }
        result = testSwitch("no");
        if (result == null || !result.equals("negative")) {
            System.out.println("FAIL: no should return negative, got " + result);
            return;
        }
        result = testSwitch("other");
        if (result == null || !result.equals("unknown")) {
            System.out.println("FAIL: other should return unknown, got " + result);
            return;
        }
        
        // Test fallthrough
        if (testWithFallthrough("Monday") != 1) {
            System.out.println("FAIL: Monday should be weekday");
            return;
        }
        if (testWithFallthrough("Friday") != 1) {
            System.out.println("FAIL: Friday should be weekday");
            return;
        }
        if (testWithFallthrough("Saturday") != 2) {
            System.out.println("FAIL: Saturday should be weekend");
            return;
        }
        if (testWithFallthrough("Holiday") != 0) {
            System.out.println("FAIL: Holiday should be default");
            return;
        }
        
        System.out.println("PASS");
    }
}

