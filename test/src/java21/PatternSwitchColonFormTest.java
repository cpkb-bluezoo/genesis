import java.util.ArrayList;
import java.util.List;

/*
 * Pattern labels in the colon form of a switch statement (JLS 14.11.1).
 */
public class PatternSwitchColonFormTest {
    record Point(int x, int y) {}

    static final List<String> log = new ArrayList<>();

    static void typePatterns(Object o) {
        switch (o) {
            case String s:
                log.add("string " + s);
                break;
            case Integer i when i > 10:
                log.add("big " + i);
                break;
            case Integer i:
                log.add("int " + i);
                break;
            default:
                log.add("other");
        }
    }

    static String recordPatterns(Object o) {
        switch (o) {
            case Point(int x, int y):
                if (x == y) {
                    return "diagonal " + x;
                }
                return "point " + x + "," + y;
            default:
                return "other";
        }
    }

    static int inLoop(Object[] items) {
        int n = 0;
        for (Object item : items) {
            switch (item) {
                case String s:
                    continue;
                default:
                    n++;
            }
            n += 10;
        }
        return n;
    }

    public static void main(String[] args) {
        typePatterns("a");
        typePatterns(11);
        typePatterns(3);
        typePatterns(2.5);
        if (!log.equals(List.of("string a", "big 11", "int 3", "other"))) {
            throw new AssertionError(log);
        }
        if (!"diagonal 2".equals(recordPatterns(new Point(2, 2))) || !"point 1,2".equals(recordPatterns(new Point(1, 2)))
                || !"other".equals(recordPatterns("p"))) {
            throw new AssertionError("recordPatterns");
        }
        if (inLoop(new Object[] {"a", 1, "b", 2}) != 22) {
            throw new AssertionError("inLoop");
        }
        System.out.println("PatternSwitchColonFormTest passed!");
    }
}
