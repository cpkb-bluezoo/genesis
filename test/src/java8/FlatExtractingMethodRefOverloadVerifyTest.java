import java.util.Collection;

/**
 * AssertJ-style overload: fixed ThrowingExtractor&lt;?, ? extends Collection&lt;?&gt;, ?&gt; vs
 * varargs ThrowingExtractor&lt;?, ?, ?&gt;... for a method reference returning Object.
 */
public class FlatExtractingMethodRefOverloadVerifyTest {

    interface ThrowingExtractor<F, T, E extends Exception> {
        T extractThrows(F f) throws E;
    }

    interface Exposable {
        Object getBean();
    }

    static final class Item implements Exposable {
        @Override
        public Object getBean() {
            return "x";
        }
    }

    static int which;

    static void fixed(ThrowingExtractor<Exposable, ? extends Collection<?>, RuntimeException> extractor) {
        which = 1;
    }

    static void varargs(ThrowingExtractor<Exposable, ?, RuntimeException>... extractors) {
        which = 2;
    }

    public static void main(String[] args) {
        which = 0;
        varargs(Exposable::getBean);
        if (which != 2) {
            throw new RuntimeException("expected varargs overload, got which=" + which);
        }
        System.out.println("PASS");
    }
}
