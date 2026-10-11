import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Function;

/* Regression test: a lambda passed where the declared parameter is the RECEIVER's
 * type variable (Consumer<Consumer<E>>.accept(T) takes a Consumer<E>) is bound to
 * what the receiver says T is - the method's own type variables are the ones a
 * lambda cannot fill. */
public class LambdaForReceiverTypeVariableParamTest<T> {
    <E> String using(BiConsumer<T, Consumer<E>> elements, Function<E, String> name, T t) {
        StringBuilder out = new StringBuilder();
        elements.accept(t, (element) -> {
            String n = name.apply(element);
            out.append(n);
        });
        return out.toString();
    }

    public static void main(String[] args) {
        LambdaForReceiverTypeVariableParamTest<String> t = new LambdaForReceiverTypeVariableParamTest<>();
        String got = t.<String>using((s, sink) -> {
            sink.accept(s + "1");
            sink.accept("2");
        }, (i) -> "<" + i + ">", "abc");
        if (!"<abc1><2>".equals(got)) {
            throw new RuntimeException(got);
        }
        System.out.println("LambdaForReceiverTypeVariableParamTest passed!");
    }
}
