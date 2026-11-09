import java.util.List;

/* "var" in a record pattern takes the type of the component it matches */
public class RecordPatternVarTest {

    record Pt(int x, String label, List<String> tags) {}

    record Pair<A, B>(A first, B second) {}

    record Measure(int i, long l) {}

    static void check(boolean ok, String what) {
        if (!ok) {
            System.out.println("FAILED: " + what);
            System.exit(1);
        }
    }

    static String describe(Object o) {
        return switch (o) {
            case Pt(var a, var b, var c) when a > 0 -> (a + 1) + ":" + b.length() + ":" + c.get(0);
            case Pt(var a, var b, var c) -> "non-positive";
            default -> "other";
        };
    }

    static String pair(Object p) {
        return switch (p) {
            case Pair<?, ?>(var f, var s) -> f + "/" + s;
            default -> "other";
        };
    }

    /* A long component takes two local variable slots */
    static String measure(Object o) {
        return switch (o) {
            case Measure(int a, long c) -> (a + c) + "";
            default -> "other";
        };
    }

    public static void main(String[] args) {
        check("4:5:a".equals(describe(new Pt(3, "label", List.of("a", "b")))), "var in a record pattern");
        check("non-positive".equals(describe(new Pt(0, "z", List.of()))), "var, guard false");
        check("other".equals(describe("x")), "no match");
        check("5".equals(measure(new Measure(2, 3L))), "long component");
        check("abc/5".equals(pair(new Pair<>("abc", 5))), "generic record, var");
    }
}
