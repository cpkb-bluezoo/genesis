import java.util.function.BiConsumer;

/*
 * Regression test: a lambda passed BEFORE the arguments that fix a generic
 * method's type variables ("calls((a, b) -> ..., p, s)") got Object
 * parameters, so "a.length()" was compiled as Object.length() and failed
 * with NoSuchMethodError. A lambda passed last was typed correctly.
 */
public class LambdaBeforeTypeVariableArgumentsTest {

    static <T, I> void lambdaFirst(BiConsumer<T, I> action, I second, T first) {
        action.accept(first, second);
    }

    static <T, I> void lambdaLast(I second, T first, BiConsumer<T, I> action) {
        action.accept(first, second);
    }

    static int total;

    public static void main(String[] args) {
        Integer number = 40;
        String text = "xy";
        lambdaFirst((a, b) -> total += a.length() + b.intValue(), number, text);
        lambdaLast(number, text, (a, b) -> total += a.length() + b.intValue());
        if (total != 84) {
            throw new RuntimeException("FAIL: expected 84 got " + total);
        }
        System.out.println("LambdaBeforeTypeVariableArgumentsTest passed!");
    }
}
