import static argmatcherlib.Matchers.arg;
import static argmatcherlib.Matchers.feed;
import static argmatcherlib.Matchers.that;

/* A generic call that returns its bare type variable and is passed as an argument ("take(arg(
 * (value) -> ...))") types the lambda's parameter by the parameter it fills, so the calls made
 * on it in turn (and the lambda they take) are typed too. */
public class ArgumentMatcherLambdaTest {
    static void take(Number ignored) {
        feed(Integer.valueOf(41));
    }

    public static void main(String[] args) {
        final StringBuilder out = new StringBuilder();
        take(arg((value) -> out.append(value.intValue())));
        take(arg((value) -> that(value).satisfying(Integer.class, (number) -> out.append(number + 1))));
        if (!"4142".equals(out.toString())) {
            throw new AssertionError(out.toString());
        }
        System.out.println("PASS");
    }
}
