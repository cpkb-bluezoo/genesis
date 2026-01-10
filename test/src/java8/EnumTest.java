// Test basic enum support
public class EnumTest {
    
    public enum Day {
        MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY, SATURDAY, SUNDAY
    }
    
    public static void main(String[] args) {
        // Test basic enum constant
        Day today = Day.MONDAY;
        System.out.println("Today is: " + today);
        
        // Test toString, ordinal, name via variable
        System.out.println("toString: " + today.toString());
        System.out.println("ordinal: " + today.ordinal());
        System.out.println("name: " + today.name());
        
        // Test values()
        Day[] days = Day.values();
        System.out.println("Number of days: " + days.length);
        
        // Test valueOf()
        Day friday = Day.valueOf("FRIDAY");
        System.out.println("valueOf(FRIDAY): " + friday);
        System.out.println("FRIDAY ordinal: " + friday.ordinal());
        
        // Verify all values with ordinals
        for (int i = 0; i < days.length; i++) {
            Day d = days[i];
            System.out.println("Day " + i + ": " + d.name() + " (ordinal=" + d.ordinal() + ")");
        }
        
        System.out.println("EnumTest passed!");
    }
}
