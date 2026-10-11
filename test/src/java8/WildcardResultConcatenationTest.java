import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Callable;

/*
 * Regression test: "\"y\" + list.get(0)" for a List<?> (the result's type is a wildcard) was
 * appended as an int (StringBuilder.append(I): VerifyError). It is an Object, or its bound.
 */
public class WildcardResultConcatenationTest {

    static class Holder<T> {
        final T value;

        Holder(T value) {
            this.value = value;
        }

        T get() {
            return this.value;
        }
    }

    static String list(List<?> list) {
        return "l" + list.get(0);
    }

    static String callable(Callable<?> callable) throws Exception {
        return "c" + callable.call();
    }

    static String holder(Holder<?> holder) {
        return "h" + holder.get();
    }

    static String bounded(Holder<? extends Number> holder) {
        return "b" + holder.get();
    }

    public static void main(String[] args) throws Exception {
        String actual = list(Arrays.asList("a")) + callable(() -> "b") + holder(new Holder<String>("c"))
            + bounded(new Holder<Integer>(4));
        if (!"lacbhcb4".equals(actual)) {
            throw new RuntimeException("FAIL: " + actual);
        }
        System.out.println("PASS");
    }
}
