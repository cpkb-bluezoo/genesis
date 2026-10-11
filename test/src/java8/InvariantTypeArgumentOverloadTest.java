import java.util.Collections;
import java.util.Iterator;

/*
 * Regression test: an overload taking "Iterable<Src>" was applicable to an argument that is a
 * Src and an Iterable<String>: the class matched and the type arguments were not compared
 * (generics are invariant), so the varargs "Src..." overload was never chosen (Spring's
 * "new Binder(new MapConfigurationPropertySource(map))": ClassCastException on the names).
 */
public class InvariantTypeArgumentOverloadTest {

    interface Src {
    }

    static class Names implements Src, Iterable<String> {
        @Override
        public Iterator<String> iterator() {
            return Collections.<String>emptyList().iterator();
        }
    }

    static class Sources implements Iterable<Src> {
        @Override
        public Iterator<Src> iterator() {
            return Collections.<Src>emptyList().iterator();
        }
    }

    static String pick(Src... sources) {
        return "varargs";
    }

    static String pick(Iterable<Src> sources) {
        return "iterable";
    }

    public static void main(String[] args) {
        String actual = pick(new Names()) + " " + pick(new Sources());
        if (!"varargs iterable".equals(actual)) {
            throw new RuntimeException("FAIL: " + actual);
        }
        System.out.println("PASS");
    }
}
