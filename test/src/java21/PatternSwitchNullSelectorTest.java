/*
 * A switch with pattern labels throws NullPointerException for a null selector
 * (JLS 14.11.3, 15.28.2).
 */
public class PatternSwitchNullSelectorTest {
    static String expr(Object o) {
        return switch (o) {
            case String s -> "s";
            default -> "d";
        };
    }

    static String stmt(Object o) {
        switch (o) {
            case String s -> {
                return "s";
            }
            default -> {
                return "d";
            }
        }
    }

    static boolean throwsNpe(Object o, boolean statement) {
        try {
            if (statement) {
                stmt(o);
            } else {
                expr(o);
            }
            return false;
        } catch (NullPointerException expected) {
            return true;
        }
    }

    public static void main(String[] args) {
        if (!throwsNpe(null, false) || !throwsNpe(null, true)) {
            throw new AssertionError("null selector did not throw");
        }
        if (!"s".equals(expr("x")) || !"d".equals(expr(1)) || !"s".equals(stmt("x")) || !"d".equals(stmt(1))) {
            throw new AssertionError("non-null selector");
        }
        System.out.println("PatternSwitchNullSelectorTest passed!");
    }
}
