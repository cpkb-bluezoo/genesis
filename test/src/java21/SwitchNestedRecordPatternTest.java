/*
 * Nested record patterns in a switch, and component patterns that do not cover their
 * component's type (a type test): each part of a case is tested in turn, and a case that
 * does not match leaves the next one to be tried.
 */
public class SwitchNestedRecordPatternTest {

    record Pt(int x, int y) {}
    record Line(Pt from, Pt end) {}
    record Holder(Object value, int tag) {}
    record Wrapper(Holder inner) {}

    static void check(boolean ok, String what) {
        if (!ok) {
            System.out.println("FAILED: " + what);
            System.exit(1);
        }
    }

    static String nested(Object o) {
        return switch (o) {
            case Line(Pt(var a, var b), Pt(int c, int d)) -> "line " + a + b + c + d;
            case Pt(int a, int b) -> "pt " + a + b;
            default -> "other";
        };
    }

    static String narrowing(Object o) {
        return switch (o) {
            case Holder(String s, int t) -> "string " + s + t;
            case Holder(Integer i, var t) -> "integer " + (i + t);
            case Holder(Object v, var t) -> "object " + v + t;
            default -> "other";
        };
    }

    static String deep(Object o) {
        return switch (o) {
            case Wrapper(Holder(String s, var t)) when t > 0 -> "positive " + s;
            case Wrapper(Holder(String s, var t)) -> "other tag " + s + t;
            case Wrapper(var h) -> "wrapper " + h.tag();
            default -> "other";
        };
    }

    static String statement(Object o) {
        String result = "unset";
        switch (o) {
            case Line(Pt(var a, var b), var rest) -> result = "line " + a + b + rest.x();
            case Holder(String s, var t) -> result = "holder " + s;
            default -> result = "other";
        }
        return result;
    }

    public static void main(String[] args) {
        check("line 1234".equals(nested(new Line(new Pt(1, 2), new Pt(3, 4)))), "nested");
        check("pt 56".equals(nested(new Pt(5, 6))), "a later case after nested");
        check("other".equals(nested(new Line(null, new Pt(3, 4)))), "null nested component does not match");
        check("other".equals(nested("x")), "other type");
        check("string hi7".equals(narrowing(new Holder("hi", 7))), "narrowing to String");
        check("integer 12".equals(narrowing(new Holder(5, 7))), "narrowing to Integer");
        check("object 2.57".equals(narrowing(new Holder(2.5, 7))), "falls through to Object");
        check("object null7".equals(narrowing(new Holder(null, 7))), "a covering pattern matches null");
        check("positive a".equals(deep(new Wrapper(new Holder("a", 1)))), "deep, guard true");
        check("other tag b0".equals(deep(new Wrapper(new Holder("b", 0)))), "deep, guard false");
        check("wrapper 3".equals(deep(new Wrapper(new Holder(9, 3)))), "deep, falls to a later case");
        check("line 125".equals(statement(new Line(new Pt(1, 2), new Pt(5, 6)))), "switch statement, nested");
        check("holder z".equals(statement(new Holder("z", 1))), "switch statement, record");
        check("other".equals(statement(new Holder(1, 1))), "switch statement, default");
    }
}
