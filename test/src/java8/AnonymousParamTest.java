import java.util.Comparator;
import java.util.function.Function;

/*
 * Parameters and locals of a method declared inside an anonymous or local
 * class belong to that method: they are not captured from the enclosing
 * method. Genuinely captured locals of the enclosing method still are.
 */
public class AnonymousParamTest {

    interface IntOp {
        int apply(int x);
    }

    interface Joiner {
        String join(String a, String b);
    }

    static void check(boolean ok, String what) {
        if (!ok) {
            System.out.println("FAILED: " + what);
            System.exit(1);
        }
    }

    public static void main(String[] args) {
        IntOp inc = new IntOp() {
            public int apply(int x) {
                return x + 1;
            }
        };
        check(inc.apply(1) == 2, "int parameter");

        Function<Integer, Integer> twice = new Function<Integer, Integer>() {
            public Integer apply(Integer x) {
                int local = x * 2;
                return local;
            }
        };
        check(twice.apply(21).intValue() == 42, "generic parameter and local");

        Comparator<String> cmp = new Comparator<String>() {
            public int compare(String p, String q) {
                return p.compareTo(q);
            }
        };
        check(cmp.compare("a", "b") < 0, "two parameters");

        final int offset = 10;
        IntOp shifted = new IntOp() {
            public int apply(int x) {
                return x + offset;
            }
        };
        check(shifted.apply(5) == 15, "capture beside a parameter");

        final String sep = "-";
        Joiner joiner = new Joiner() {
            public String join(String a, String b) {
                return a + sep + b;
            }
        };
        check("x-y".equals(joiner.join("x", "y")), "captured separator");

        class Local implements IntOp {
            public int apply(int x) {
                return x * offset;
            }
        }
        check(new Local().apply(3) == 30, "local class");

        System.out.println("AnonymousParamTest passed!");
    }
}
