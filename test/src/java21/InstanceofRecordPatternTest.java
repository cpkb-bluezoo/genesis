import java.util.List;

/*
 * Record patterns with instanceof (JEP 440): the variables are in scope where the
 * test is known to have succeeded, a nested pattern is tested in turn, and a
 * component pattern that does not cover its component's type is a type test.
 */
public class InstanceofRecordPatternTest {

    record Pt(int x, int y) {}
    record Named(String name, Pt at) {}
    record Holder(Object value, int tag) {}
    record Wide(int i, long l) {}
    record Tagged(List<String> tags, String label) {}
    record Line(Pt from, Pt end) {}

    static void check(boolean ok, String what) {
        if (!ok) {
            System.out.println("FAILED: " + what);
            System.exit(1);
        }
    }

    static String simple(Object o) {
        if (o instanceof Pt(int a, int b)) {
            return "pt " + a + "," + b;
        }
        return "no";
    }

    static String inferred(Object o) {
        if (o instanceof Pt(var a, var b) && a < b) {
            return "ordered " + (a + b);
        }
        return "no";
    }

    static String negated(Object o) {
        if (!(o instanceof Named(String n, var at))) {
            return "no";
        }
        return n + "@" + at.x();
    }

    static String nested(Object o) {
        if (o instanceof Line(Pt(var a, var b), Pt(int c, int d))) {
            return "line " + a + b + c + d;
        }
        return "no";
    }

    static String narrowing(Object o) {
        if (o instanceof Holder(String s, int t)) {
            return "string " + s + t;
        } else if (o instanceof Holder(Integer i, var t)) {
            return "integer " + (i + t);
        } else if (o instanceof Holder(Object v, var t)) {
            return "object " + v + t;
        }
        return "no";
    }

    static String wide(Object o) {
        return o instanceof Wide(int a, long b) ? "wide " + (a + b) : "no";
    }

    static String generic(Object o) {
        if (o instanceof Tagged(var tags, var label) && !tags.isEmpty()) {
            return label + tags.get(0);
        }
        return "no";
    }

    static int loop(List<Object> items) {
        int total = 0;
        int i = 0;
        while (i < items.size() && items.get(i) instanceof Pt(int a, int b)) {
            total += a * b;
            i++;
        }
        return total;
    }

    public static void main(String[] args) {
        check("pt 1,2".equals(simple(new Pt(1, 2))), "simple");
        check("no".equals(simple("x")), "simple, other type");
        check("no".equals(simple(null)), "simple, null");
        check("ordered 3".equals(inferred(new Pt(1, 2))), "var with &&");
        check("no".equals(inferred(new Pt(2, 1))), "var with &&, false");
        check("ann@4".equals(negated(new Named("ann", new Pt(4, 5)))), "negated, binding after the if");
        check("no".equals(negated(new Pt(1, 2))), "negated, other type");
        check("line 1234".equals(nested(new Line(new Pt(1, 2), new Pt(3, 4)))), "nested");
        check("no".equals(nested(new Line(null, new Pt(3, 4)))), "nested, null component does not match");
        check("string hi7".equals(narrowing(new Holder("hi", 7))), "narrowing to String");
        check("integer 12".equals(narrowing(new Holder(5, 7))), "narrowing to Integer");
        check("object null7".equals(narrowing(new Holder(null, 7))), "a total pattern matches null");
        check("wide 5".equals(wide(new Wide(2, 3L))), "long component");
        check("a1".equals(generic(new Tagged(List.of("1"), "a"))), "generic component, var");
        check(loop(List.of(new Pt(1, 2), new Pt(3, 4), "stop", new Pt(9, 9))) == 14, "loop");
    }
}
