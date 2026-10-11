import java.util.ArrayList;
import java.util.List;
import java.util.function.BiPredicate;
import java.util.function.Predicate;
import java.util.function.Supplier;

/* Regression test: lambdas and method references as the operands of "?:" take
 * the type of where the conditional goes (a field, a return, a variable), and
 * the stack that is under them is still typed for the frames after
 * ("this.filter = (filter != null) ? filter : (t) -> true" in Spring Boot's
 * StandardStackTracePrinter); and the array constructor reference of
 * List.toArray(X[]::new) is an IntFunction, never the T[] overload. */
public class ConditionalLambdaOperandsTest {
    final Predicate<String> filter;
    final BiPredicate<Integer, String> frameFilter;

    ConditionalLambdaOperandsTest(Predicate<String> filter, BiPredicate<Integer, String> frameFilter) {
        this.filter = (filter != null) ? filter : (t) -> true;
        this.frameFilter = (frameFilter != null) ? frameFilter : (i, t) -> false;
    }

    static Supplier<String> pick(boolean b) {
        return b ? () -> "a" : () -> "b";
    }

    String name() {
        return "n";
    }

    Supplier<String> ref(boolean b) {
        return b ? this::name : String::new;
    }

    public static void main(String[] args) {
        ConditionalLambdaOperandsTest t = new ConditionalLambdaOperandsTest(null, null);
        ConditionalLambdaOperandsTest u = new ConditionalLambdaOperandsTest((s) -> false, null);
        List<String> l = new ArrayList<>(List.of("x", "y"));
        String[] arr = l.toArray(String[]::new);
        if (!t.filter.test("x") || u.filter.test("x") || t.frameFilter.test(1, "y")
                || !"a".equals(pick(true).get()) || !"b".equals(pick(false).get())
                || !"n".equals(t.ref(true).get()) || !"".equals(t.ref(false).get())
                || arr.length != 2 || !"y".equals(arr[1])) {
            throw new RuntimeException("conditional lambdas");
        }
        System.out.println("ConditionalLambdaOperandsTest passed!");
    }
}
