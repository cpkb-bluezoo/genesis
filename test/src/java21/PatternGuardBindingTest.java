/*
 * A "when" guard sees the variables its own pattern binds (JLS 14.11.1, 6.3.4).
 */
public class PatternGuardBindingTest {
    record Point(int x, int y) {}

    static String typeGuard(Object o) {
        return switch (o) {
            case Integer i when i > 10 -> "big " + i;
            case Integer i -> "int " + i;
            case String s when s.isEmpty() -> "empty";
            case String s -> "string " + s;
            default -> "other";
        };
    }

    static String recordGuard(Object o) {
        return switch (o) {
            case Point(int x, int y) when x == y -> "diagonal " + x;
            case Point(int x, int y) -> "point " + x + "," + y;
            default -> "other";
        };
    }

    static String statementGuard(Object o) {
        switch (o) {
            case Integer i when i < 0 -> {
                return "negative";
            }
            case Integer i -> {
                return "int";
            }
            default -> {
                return "other";
            }
        }
    }

    public static void main(String[] args) {
        if (!"big 11".equals(typeGuard(11)) || !"int 3".equals(typeGuard(3))
                || !"empty".equals(typeGuard("")) || !"string x".equals(typeGuard("x"))
                || !"other".equals(typeGuard(1.5))) {
            throw new AssertionError("typeGuard");
        }
        if (!"diagonal 2".equals(recordGuard(new Point(2, 2))) || !"point 1,2".equals(recordGuard(new Point(1, 2)))
                || !"other".equals(recordGuard("p"))) {
            throw new AssertionError("recordGuard");
        }
        if (!"negative".equals(statementGuard(-1)) || !"int".equals(statementGuard(1))
                || !"other".equals(statementGuard("s"))) {
            throw new AssertionError("statementGuard");
        }
        System.out.println("PatternGuardBindingTest passed!");
    }
}
