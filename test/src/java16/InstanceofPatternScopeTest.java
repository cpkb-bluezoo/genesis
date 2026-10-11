import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/* Regression test for pattern matching for instanceof (JLS 15.20.2, 6.3.1): a
 * binding is in scope wherever the test is known to have succeeded, not only
 * in the then branch of an if whose whole condition is the instanceof. Each
 * shape below failed with "Cannot resolve symbol: s" (or, as a bare if,
 * evaluated the source expression twice). */
public class InstanceofPatternScopeTest {
    int field = 7;
    String s = "field";
    static int calls;

    static Object next(Object o) {
        calls++;
        return o;
    }

    // && in a return, a local initializer, an argument
    static boolean andReturn(Object o) {
        return o instanceof String s && s.length() > 2;
    }

    static boolean andLocal(Object o) {
        boolean b = o instanceof Integer i && i > 3;
        return b;
    }

    static String andArgument(Object o) {
        return String.valueOf(o instanceof String s && s.isEmpty());
    }

    // || with a negated test: the binding is in scope on the right
    static boolean orNegated(Object o) {
        return !(o instanceof String s) || s.isEmpty();
    }

    // a conditional expression
    static int ternary(Object o) {
        return o instanceof Integer i ? i + 1 : -1;
    }

    static String ternaryElse(Object o) {
        return !(o instanceof String s) ? "not a string" : s.toUpperCase();
    }

    // chain and nesting: the second test uses the first binding
    static String chain(Object o) {
        if (o instanceof List<?> l && !l.isEmpty() && l.get(0) instanceof String first && first.length() > 1) {
            return "list:" + first;
        } else if (o instanceof String s && s.length() > 1) {
            return "string:" + s;
        } else if (o instanceof Integer s) {
            return "int:" + s;
        }
        return "other";
    }

    // the source expression is evaluated once
    static String once(Object o) {
        calls = 0;
        if (next(o) instanceof String s) {
            return s + calls;
        }
        return "no" + calls;
    }

    // flow scoping: the binding is in scope after an if that cannot complete normally
    static int early(Object o) {
        if (!(o instanceof String s)) {
            return -1;
        }
        return s.length();
    }

    static int earlyElse(Object o) {
        if (o instanceof String s) {
            return s.length();
        } else {
            return -2;
        }
    }

    static int earlyThrow(Object o) {
        if (!(o instanceof Integer n)) {
            throw new IllegalArgumentException("not an int");
        }
        return n * 2;
    }

    // loops
    static String whileLoop(Iterator<Object> it) {
        StringBuilder sb = new StringBuilder();
        Object cur;
        while ((cur = it.next()) instanceof String s && !s.isEmpty()) {
            sb.append(s).append(',');
        }
        return sb.toString();
    }

    static int whileNot(Iterator<Object> it) {
        int n = 0;
        while (it.hasNext() && it.next() instanceof Integer i) {
            n += i;
        }
        return n;
    }

    static int forLoop(List<Object> items) {
        int n = 0;
        for (int i = 0; i < items.size() && items.get(i) instanceof String s; i++) {
            n += s.length();
        }
        return n;
    }

    static int doLoop(List<Object> items) {
        int i = 0;
        int n = 0;
        Object o;
        do {
            o = items.get(i++);
            if (o instanceof Integer k) {
                n += k;
            }
        } while (i < items.size());
        return n;
    }

    // a lambda and an anonymous class capture a binding
    static boolean captured(Object o, List<String> names) {
        return o instanceof String s && names.stream().anyMatch(x -> x.equals(s));
    }

    // narrower than the static type; a wrapper; a field with the same name
    boolean narrower(CharSequence cs) {
        return cs instanceof String s && s.length() == this.field && this.s.equals("field");
    }

    // same name reused in separate statements
    static String reuse(Object a, Object b) {
        String r = "";
        if (a instanceof String x) {
            r += x;
        }
        if (b instanceof String x) {
            r += x;
        }
        return r;
    }

    public static void main(String[] args) {
        check(andReturn("abc"), true);
        check(andReturn("ab"), false);
        check(andReturn(5), false);
        check(andLocal(9), true);
        check(andLocal("x"), false);
        check(andArgument(""), "true");
        check(andArgument(4), "false");
        check(orNegated(1), true);
        check(orNegated(""), true);
        check(orNegated("x"), false);
        check(ternary(4), 5);
        check(ternary("x"), -1);
        check(ternaryElse(3), "not a string");
        check(ternaryElse("ab"), "AB");
        check(chain(List.of("ab")), "list:ab");
        check(chain("xy"), "string:xy");
        check(chain(3), "int:3");
        check(chain(List.of()), "other");
        check(once("v"), "v1");
        check(once(1), "no1");
        check(early("four"), 4);
        check(early(4), -1);
        check(earlyElse("ab"), 2);
        check(earlyElse(1), -2);
        check(earlyThrow(21), 42);
        try {
            earlyThrow("x");
            throw new RuntimeException("no exception");
        } catch (IllegalArgumentException expected) {
            // expected
        }
        List<Object> items = new ArrayList<>(List.of("a", "bb", "", "ccc"));
        check(whileLoop(items.iterator()), "a,bb,");
        check(whileNot(new ArrayList<Object>(List.of(1, 2, 3, "x", 4)).iterator()), 6);
        check(forLoop(new ArrayList<Object>(List.of("a", "bb", 3, "cccc"))), 3);
        check(doLoop(new ArrayList<Object>(List.of(1, "x", 2, 3))), 6);
        check(captured("n", List.of("m", "n")), true);
        check(captured("z", List.of("m", "n")), false);
        check(new InstanceofPatternScopeTest().narrower("1234567"), true);
        check(new InstanceofPatternScopeTest().narrower("12"), false);
        check(reuse("a", "b"), "ab");
        check(reuse(1, "b"), "b");
        System.out.println("InstanceofPatternScopeTest passed!");
    }

    static void check(Object got, Object want) {
        if (!want.equals(got)) {
            throw new RuntimeException("got " + got + ", want " + want);
        }
    }
}
