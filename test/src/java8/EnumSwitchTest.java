/**
 * Test enum in switch statements.
 */
public class EnumSwitchTest {
    
    public enum Day {
        MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY, SATURDAY, SUNDAY
    }
    
    public static int getDayNumber(Day day) {
        switch (day) {
            case MONDAY:
                return 1;
            case TUESDAY:
                return 2;
            case WEDNESDAY:
                return 3;
            case THURSDAY:
                return 4;
            case FRIDAY:
                return 5;
            case SATURDAY:
                return 6;
            case SUNDAY:
                return 7;
            default:
                return 0;
        }
    }
    
    public static void main(String[] args) {
        // Test getDayNumber using fully-qualified class name
        if (EnumSwitchTest.getDayNumber(Day.MONDAY) != 1) {
            throw new RuntimeException("Monday should be 1");
        }
        if (EnumSwitchTest.getDayNumber(Day.TUESDAY) != 2) {
            throw new RuntimeException("Tuesday should be 2");
        }
        if (EnumSwitchTest.getDayNumber(Day.WEDNESDAY) != 3) {
            throw new RuntimeException("Wednesday should be 3");
        }
        if (EnumSwitchTest.getDayNumber(Day.THURSDAY) != 4) {
            throw new RuntimeException("Thursday should be 4");
        }
        if (EnumSwitchTest.getDayNumber(Day.FRIDAY) != 5) {
            throw new RuntimeException("Friday should be 5");
        }
        if (EnumSwitchTest.getDayNumber(Day.SATURDAY) != 6) {
            throw new RuntimeException("Saturday should be 6");
        }
        if (EnumSwitchTest.getDayNumber(Day.SUNDAY) != 7) {
            throw new RuntimeException("Sunday should be 7");
        }
        
        // Test all days
        Day[] days = Day.values();
        for (int i = 0; i < days.length; i++) {
            Day d = days[i];
            System.out.println(d.name() + ": number=" + EnumSwitchTest.getDayNumber(d));
        }
        
        System.out.println("EnumSwitchTest passed!");
    }
}
