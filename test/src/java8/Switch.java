/**
 * Test switch statement.
 */
public class Switch {
    
    public static int dayType(int day) {
        int type;
        switch (day) {
            case 1:
            case 7:
                type = 0;  // Weekend
                break;
            case 2:
            case 3:
            case 4:
            case 5:
            case 6:
                type = 1;  // Weekday
                break;
            default:
                type = -1;  // Invalid
                break;
        }
        return type;
    }
    
    public static int monthDays(int month) {
        int days;
        switch (month) {
            case 1: case 3: case 5: case 7: case 8: case 10: case 12:
                days = 31;
                break;
            case 4: case 6: case 9: case 11:
                days = 30;
                break;
            case 2:
                days = 28;
                break;
            default:
                days = 0;
                break;
        }
        return days;
    }
}

