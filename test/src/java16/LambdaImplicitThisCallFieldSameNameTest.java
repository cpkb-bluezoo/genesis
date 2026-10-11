import java.util.Collection;
import java.util.List;
import java.util.Map;

/* Regression test: a lambda calling an instance method without a receiver captures
 * this even when the class has a FIELD of the same name as the method (Spring
 * Boot's ProfilesValidator: "private final boolean validate" and
 * "validate(Object)"), and failed with "cannot call instance method without
 * receiver in static context". */
public class LambdaImplicitThisCallFieldSameNameTest {
    private final boolean validate = true;
    private int seen;

    private void validate(Object value) {
        if (!this.validate) {
            return;
        }
        if (value instanceof Collection<?> list) {
            list.forEach(this::validate);
            return;
        }
        if (value instanceof Map<?, ?> map) {
            map.forEach((k, v) -> validate(v));
            return;
        }
        seen++;
    }

    public static void main(String[] args) {
        LambdaImplicitThisCallFieldSameNameTest t = new LambdaImplicitThisCallFieldSameNameTest();
        t.validate(List.of(1, Map.of("a", 2), List.of(3, 4)));
        if (t.seen != 4) {
            throw new RuntimeException("seen " + t.seen);
        }
        System.out.println("LambdaImplicitThisCallFieldSameNameTest passed!");
    }
}
