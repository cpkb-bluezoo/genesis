/* Regression test for arrow-form switch rules whose label is a single
 * identifier (an enum constant or a constant variable): "case RED -> 1" was
 * parsed as the lambda "RED -> 1", the switch lost its closing brace and the
 * rest of the file failed with "Expected type declaration". */
public class ArrowCaseConstantLabelTest {
    enum Colour { RED, GREEN, BLUE }

    static final int ONE = 1;
    static final int TWO = 2;

    static int expr(Colour c) {
        return switch (c) {
            case RED -> 10;
            case GREEN, BLUE -> 20;
        };
    }

    static String wrapped(Colour c) {
        return switch (c) {
            case RED ->
                new String("r");
            case GREEN -> {
                String s = "g";
                yield s;
            }
            default -> "other";
        };
    }

    static int constants(int n) {
        return switch (n) {
            case ONE -> 100;
            case TWO -> 200;
            default -> 0;
        };
    }

    static int statement(Colour c) {
        int r = 0;
        switch (c) {
            case RED -> r = 1;
            case GREEN -> r = 2;
            default -> r = 3;
        }
        return r;
    }

    private static void check(boolean ok, String what) {
        if (!ok) {
            throw new AssertionError(what);
        }
    }

    public static void main(String[] args) {
        check(expr(Colour.RED) == 10 && expr(Colour.GREEN) == 20 && expr(Colour.BLUE) == 20, "expr");
        check(wrapped(Colour.RED).equals("r") && wrapped(Colour.GREEN).equals("g")
                && wrapped(Colour.BLUE).equals("other"), "wrapped");
        check(constants(1) == 100 && constants(2) == 200 && constants(3) == 0, "constants");
        check(statement(Colour.RED) == 1 && statement(Colour.GREEN) == 2 && statement(Colour.BLUE) == 3, "statement");
        System.out.println("ArrowCaseConstantLabelTest passed!");
    }
}
