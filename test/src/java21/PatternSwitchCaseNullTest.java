/*
 * "case null" in a switch with pattern labels (JLS 14.11.1): it matches a null
 * selector, wherever it stands among the labels, and may be joined to default.
 */
public class PatternSwitchCaseNullTest {
    static String first(Object o) {
        return switch (o) {
            case null -> "null";
            case String s -> "string " + s;
            default -> "other";
        };
    }

    static String middle(Object o) {
        return switch (o) {
            case String s -> "string " + s;
            case null -> "null";
            default -> "other";
        };
    }

    static String last(Object o) {
        return switch (o) {
            case String s -> "string " + s;
            case Integer i -> "int " + i;
            case null -> "null";
            default -> "other";
        };
    }

    static String block(Object o) {
        return switch (o) {
            case null -> {
                String r = "n";
                yield r + "ull";
            }
            case String s -> "string " + s;
            default -> "other";
        };
    }

    static String statement(Object o) {
        switch (o) {
            case null -> {
                return "null";
            }
            case String s -> {
                return "string " + s;
            }
            default -> {
                return "other";
            }
        }
    }

    static String nullOrDefault(Object o) {
        return switch (o) {
            case String s -> "string " + s;
            case null, default -> "nd";
        };
    }

    static String nullOrDefaultStatement(Object o) {
        switch (o) {
            case String s -> {
                return "string " + s;
            }
            case null, default -> {
                return "nd";
            }
        }
    }

    static String nullOrDefaultColon(Object o) {
        switch (o) {
            case String s:
                return "string " + s;
            case null, default:
                return "nd";
        }
    }

    public static void main(String[] args) {
        if (!"null".equals(first(null)) || !"string a".equals(first("a")) || !"other".equals(first(1))) {
            throw new AssertionError("first");
        }
        if (!"null".equals(middle(null)) || !"string a".equals(middle("a")) || !"other".equals(middle(1))) {
            throw new AssertionError("middle");
        }
        if (!"null".equals(last(null)) || !"string a".equals(last("a")) || !"int 1".equals(last(1))
                || !"other".equals(last(2.5))) {
            throw new AssertionError("last");
        }
        if (!"null".equals(block(null)) || !"string a".equals(block("a")) || !"other".equals(block(1))) {
            throw new AssertionError("block");
        }
        if (!"null".equals(statement(null)) || !"string a".equals(statement("a")) || !"other".equals(statement(1))) {
            throw new AssertionError("statement");
        }
        if (!"nd".equals(nullOrDefault(null)) || !"string a".equals(nullOrDefault("a")) || !"nd".equals(nullOrDefault(1))) {
            throw new AssertionError("nullOrDefault");
        }
        if (!"nd".equals(nullOrDefaultStatement(null)) || !"string a".equals(nullOrDefaultStatement("a"))
                || !"nd".equals(nullOrDefaultStatement(1))) {
            throw new AssertionError("nullOrDefaultStatement");
        }
        if (!"nd".equals(nullOrDefaultColon(null)) || !"string a".equals(nullOrDefaultColon("a"))
                || !"nd".equals(nullOrDefaultColon(1))) {
            throw new AssertionError("nullOrDefaultColon");
        }
        System.out.println("PatternSwitchCaseNullTest passed!");
    }
}
