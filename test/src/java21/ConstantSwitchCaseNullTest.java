/*
 * "case null" in a switch over a String, an enum or a boxed integer (JLS 14.11.1):
 * a null selector selects it; without one, a null selector throws.
 */
public class ConstantSwitchCaseNullTest {
    enum Color { RED, GREEN }

    static String stringExpr(String s) {
        return switch (s) {
            case null -> "null";
            case "a" -> "A";
            default -> "other";
        };
    }

    static String stringExprJoined(String s) {
        return switch (s) {
            case "a" -> "A";
            case null, default -> "nd";
        };
    }

    static String enumExpr(Color c) {
        return switch (c) {
            case null -> "null";
            case RED -> "red";
            case GREEN -> "green";
        };
    }

    static String enumExprJoined(Color c) {
        return switch (c) {
            case RED -> "red";
            case null, default -> "nd";
        };
    }

    static String boxedExpr(Integer i) {
        return switch (i) {
            case null -> "null";
            case 1 -> "one";
            default -> "other";
        };
    }

    static String stringStatement(String s) {
        switch (s) {
            case null -> {
                return "null";
            }
            case "a" -> {
                return "A";
            }
            default -> {
                return "other";
            }
        }
    }

    static String stringStatementJoined(String s) {
        switch (s) {
            case "a" -> {
                return "A";
            }
            case null, default -> {
                return "nd";
            }
        }
    }

    static String enumStatementColon(Color c) {
        switch (c) {
            case RED:
                return "red";
            case null:
                return "null";
            default:
                return "other";
        }
    }

    static String boxedStatement(Integer i) {
        switch (i) {
            case null:
                return "null";
            case 1:
                return "one";
            default:
                return "other";
        }
    }

    static String boxedStatementNoNull(Integer i) {
        switch (i) {
            case 1:
                return "one";
            default:
                return "other";
        }
    }

    static boolean throwsNpe(String s) {
        int n = 0;
        try {
            switch (s) {
                case "a":
                    n = 1;
                    break;
                default:
                    n = 2;
            }
        } catch (NullPointerException expected) {
            return true;
        }
        return n < 0;
    }

    public static void main(String[] args) {
        if (!"null".equals(stringExpr(null)) || !"A".equals(stringExpr("a")) || !"other".equals(stringExpr("z"))) {
            throw new AssertionError("stringExpr");
        }
        if (!"nd".equals(stringExprJoined(null)) || !"A".equals(stringExprJoined("a")) || !"nd".equals(stringExprJoined("z"))) {
            throw new AssertionError("stringExprJoined");
        }
        if (!"null".equals(enumExpr(null)) || !"red".equals(enumExpr(Color.RED)) || !"green".equals(enumExpr(Color.GREEN))) {
            throw new AssertionError("enumExpr");
        }
        if (!"nd".equals(enumExprJoined(null)) || !"red".equals(enumExprJoined(Color.RED))
                || !"nd".equals(enumExprJoined(Color.GREEN))) {
            throw new AssertionError("enumExprJoined");
        }
        if (!"null".equals(boxedExpr(null)) || !"one".equals(boxedExpr(1)) || !"other".equals(boxedExpr(2))) {
            throw new AssertionError("boxedExpr");
        }
        if (!"null".equals(stringStatement(null)) || !"A".equals(stringStatement("a")) || !"other".equals(stringStatement("z"))) {
            throw new AssertionError("stringStatement");
        }
        if (!"nd".equals(stringStatementJoined(null)) || !"A".equals(stringStatementJoined("a"))
                || !"nd".equals(stringStatementJoined("z"))) {
            throw new AssertionError("stringStatementJoined");
        }
        if (!"null".equals(enumStatementColon(null)) || !"red".equals(enumStatementColon(Color.RED))
                || !"other".equals(enumStatementColon(Color.GREEN))) {
            throw new AssertionError("enumStatementColon");
        }
        if (!"null".equals(boxedStatement(null)) || !"one".equals(boxedStatement(1)) || !"other".equals(boxedStatement(2))) {
            throw new AssertionError("boxedStatement");
        }
        if (!"one".equals(boxedStatementNoNull(1)) || !"other".equals(boxedStatementNoNull(2))) {
            throw new AssertionError("boxedStatementNoNull");
        }
        if (!throwsNpe(null)) {
            throw new AssertionError("a switch with no case null must throw for null");
        }
        System.out.println("ConstantSwitchCaseNullTest passed!");
    }
}
