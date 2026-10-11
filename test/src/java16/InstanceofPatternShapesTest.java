import java.util.ArrayDeque;
import java.util.Deque;
import java.util.List;
import java.util.Map;
import java.util.function.Supplier;

/* More shapes of pattern matching for instanceof: scoping after loops, a ||
 * of && groups, arrays, generics, nesting, initializers, and bindings used from
 * lambdas and anonymous classes. */
public class InstanceofPatternShapesTest {
    static final Object CONSTANT = "constant";
    static final boolean CONSTANT_IS_STRING = CONSTANT instanceof String cs && !cs.isEmpty();
    final boolean instanceFlag = CONSTANT instanceof String is && is.length() == 8;

    static int calls;

    static Object tick(Object o) {
        calls++;
        return o;
    }

    // after a while loop that is not left by a break, the binding of the false condition
    static String untilString(Deque<Object> q) {
        Object o = q.poll();
        while (!(o instanceof String s)) {
            o = q.poll();
        }
        return s;
    }

    static String untilStringDo(Deque<Object> q) {
        Object o;
        do {
            o = q.poll();
        } while (!(o instanceof String s));
        return "got " + o;
    }

    // the binding of a for loop's false condition, after it; a final binding
    static String forAfter(Deque<Object> q) {
        Object o = q.poll();
        for (; !(o instanceof final String s); o = q.poll()) {
            // empty
        }
        return s + "?";
    }

    // a binding in a lambda body
    static long inLambda(List<Object> items) {
        return items.stream().filter(x -> x instanceof String s && s.length() > 1).count();
    }

    // a || of && groups: each binding is in scope only in its own group
    static String either(Object a, Object b) {
        if ((a instanceof Integer i && i > 10) || (b instanceof String s && s.length() > 2)) {
            return "yes";
        }
        return "no";
    }

    // arrays and generics
    static int arrays(Object o) {
        if (o instanceof int[] ints && ints.length > 0) {
            return ints[0];
        }
        if (o instanceof String[] strings) {
            return strings.length;
        }
        if (o instanceof Map<?, ?> m && !m.isEmpty()) {
            return m.size() * 100;
        }
        return -1;
    }

    // nested ternaries
    static String nested(Object o) {
        return o instanceof Integer i ? (i > 5 ? "big" + i : "small" + i)
             : o instanceof String s ? "str" + s : "other";
    }

    // negated and && inside ||, flow scoping after
    static int guard(Object o) {
        if (!(o instanceof String s) || s.isEmpty()) {
            return -1;
        }
        return s.length();
    }

    // an else branch uses a binding of the negated test
    static String negatedElse(Object o) {
        if (!(o instanceof Integer n)) {
            return "not";
        } else {
            return "n" + (n + 1);
        }
    }

    // the loop variable is a binding of the loop condition and the update uses it
    static int forUpdate(List<Object> items) {
        int total = 0;
        for (int i = 0; i < items.size() && items.get(i) instanceof Integer k; i += k > 0 ? 1 : 2) {
            total += k;
        }
        return total;
    }

    // a lambda and an anonymous class capture the binding, in a nested expression
    static Supplier<String> lambda(Object o) {
        if (o instanceof String s) {
            return () -> "L" + s;
        }
        return () -> "none";
    }

    static Supplier<String> anonymous(Object o) {
        if (o instanceof String s && !s.isEmpty()) {
            return new Supplier<String>() {
                public String get() {
                    return "A" + s;
                }
            };
        }
        return null;
    }

    // evaluated once, even for a chained test
    static String chain(Object o) {
        calls = 0;
        if (tick(o) instanceof List<?> l && tick(l.get(0)) instanceof String first) {
            return first + calls;
        }
        return "no" + calls;
    }

    // in an argument and as the operand of a comparison
    static boolean arg(Object o) {
        return Boolean.valueOf(o instanceof String s && s.isEmpty()) == Boolean.TRUE;
    }

    static boolean cmp(Object o) {
        return (o instanceof Integer i && i > 3) == true;
    }

    // used in string concatenation and compound assignment
    static String concat(Object o) {
        String r = "";
        if (o instanceof Integer i) {
            r += i;
            r += "+" + i;
        }
        return r;
    }

    // the same name in a later statement is a different binding
    static String sequential(Object a) {
        String r = "";
        if (a instanceof String x) {
            r += x;
        }
        if (a instanceof CharSequence x) {
            r += x.length();
        }
        for (Object o : new Object[] {1, "q"}) {
            if (o instanceof String x) {
                r += x;
            }
        }
        return r;
    }

    public static void main(String[] args) {
        check(CONSTANT_IS_STRING, true);
        check(new InstanceofPatternShapesTest().instanceFlag, true);
        check(untilString(new ArrayDeque<Object>(List.of(1, 2, "x", "y"))), "x");
        check(untilStringDo(new ArrayDeque<Object>(List.of(1, "z"))), "got z");
        check(forAfter(new ArrayDeque<Object>(List.of(1, 2.5, "f"))), "f?");
        check(inLambda(List.of("a", "bb", 3, "ccc")), 2L);
        check(either(20, null), "yes");
        check(either(1, "abc"), "yes");
        check(either(1, "ab"), "no");
        check(arrays(new int[] {9}), 9);
        check(arrays(new String[] {"a", "b"}), 2);
        check(arrays(Map.of(1, 2)), 100);
        check(arrays(3), -1);
        check(nested(9), "big9");
        check(nested(2), "small2");
        check(nested("s"), "strs");
        check(nested(1.5), "other");
        check(guard("abc"), 3);
        check(guard(""), -1);
        check(guard(5), -1);
        check(negatedElse(1), "n2");
        check(negatedElse("x"), "not");
        check(forUpdate(List.of(1, 2, 3)), 6);
        check(forUpdate(List.of(1, "x", 3)), 1);
        check(lambda("v").get(), "Lv");
        check(lambda(1).get(), "none");
        check(anonymous("w").get(), "Aw");
        check(chain(List.of("p")), "p2");
        check(chain("no list"), "no1");
        check(arg(""), true);
        check(arg("x"), false);
        check(cmp(5), true);
        check(cmp(1), false);
        check(concat(7), "7+7");
        check(sequential("ab"), "ab2q");
        System.out.println("InstanceofPatternShapesTest passed!");
    }

    static void check(Object got, Object want) {
        if (!want.equals(got)) {
            throw new RuntimeException("got " + got + ", want " + want);
        }
    }
}
