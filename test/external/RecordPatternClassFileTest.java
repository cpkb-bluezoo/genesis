import recordlib.Box;
import recordlib.Pt;

/*
 * Record patterns on records loaded from class files: a component is matched by
 * its position, whatever the pattern variable is called, and "var" takes the
 * component's type.
 */
public class RecordPatternClassFileTest {

    static void check(boolean ok, String what) {
        if (!ok) {
            System.out.println("FAILED: " + what);
            System.exit(1);
        }
    }

    static String typed(Object o) {
        return switch (o) {
            case Pt(int a, String b, java.util.List<String> c) -> a + ":" + b + ":" + c.size();
            default -> "other";
        };
    }

    static String inferred(Object o) {
        return switch (o) {
            case Pt(var a, var b, var c) when a > 0 -> (a + 1) + ":" + b.length() + ":" + c.get(0);
            default -> "other";
        };
    }

    static String generic(Object o) {
        return switch (o) {
            case Box<?>(var v, var n) -> v + ":" + (n + 1);
            default -> "other";
        };
    }

    static String tested(Object o) {
        if (o instanceof Pt(var a, String b, var c) && !c.isEmpty()) {
            return a + ":" + b + ":" + c.get(0);
        }
        return "other";
    }

    public static void main(String[] args) {
        Pt p = new Pt(3, "label", java.util.List.of("a", "b"));
        check("3:label:2".equals(typed(p)), "explicit types, other names");
        check("4:5:a".equals(inferred(p)), "var components");
        check("hello:8".equals(generic(new Box<String>("hello", 7L))), "generic record, var");
        check("other".equals(typed("x")), "no match");
        check("3:label:a".equals(tested(p)), "instanceof, other names, var");
        check("other".equals(tested("x")), "instanceof, no match");
    }
}
