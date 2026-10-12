import java.util.Collection;
import java.util.Collections;
import java.util.function.Function;

/**
 * Regression: method ref to interface method returning Object must keep Object in
 * bootstrap/specialized SAM (not a narrower override return type).
 */
public class MethodRefFlatExtractObjectReturnVerifyTest {

    interface Exposable {
        Object getBean();
    }

    static final class Item implements Exposable {
        @Override
        public Collection<?> getBean() {
            return Collections.singletonList("x");
        }
    }

    public static void main(String[] args) {
        Function<Exposable, ?> fn = Exposable::getBean;
        Object o = fn.apply(new Item());
        if (!(o instanceof Collection)) {
            throw new RuntimeException("expected Collection, got " + o);
        }
        System.out.println("PASS");
    }
}
