import java.util.ArrayList;
import java.util.List;

/* Regression test: for "<T> T[] toArray(T[] a)" T is inferred from the
 * ARGUMENT, not from the receiver's element type. Passing a wider array
 * (new Number[0]) to a List<Integer>'s toArray yields a Number[]; genesis
 * took T from the receiver and emitted checkcast Integer[], failing with
 * ClassCastException at run time. Mirrors gumdrop's
 * QuicSecurityInfo.parsePeerCerts():
 * "List<X509Certificate> chain; return chain.toArray(new Certificate[0]);". */
public class ToArrayArgumentInferenceVerifyTest {
    static Number[] widen(List<Integer> list) {
        return list.toArray(new Number[0]);
    }

    static Object[] widest(List<String> list) {
        return list.toArray(new Object[0]);
    }

    public static void main(String[] args) {
        List<Integer> ints = new ArrayList<Integer>();
        ints.add(4);
        ints.add(5);
        Number[] n = widen(ints);
        if (n.length != 2 || n[1].intValue() != 5) {
            throw new RuntimeException("widen");
        }
        if (n.getClass() != Number[].class) {
            throw new RuntimeException("wrong runtime type " + n.getClass());
        }
        List<String> strs = new ArrayList<String>();
        strs.add("x");
        Object[] o = widest(strs);
        if (o.length != 1 || !"x".equals(o[0])) {
            throw new RuntimeException("widest");
        }
        Integer[] exact = ints.toArray(new Integer[0]);
        if (exact.length != 2) {
            throw new RuntimeException("exact");
        }
        System.out.println("ToArrayArgumentInferenceVerifyTest passed!");
    }
}
