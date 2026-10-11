import java.util.function.Function;
import java.util.function.Supplier;

/* Regression test: a lambda whose block body declares a local and returns it
 * where the interface's return type is still a type variable ("Supplier<T>"):
 * the return expression was typed before the local existed and reported
 * "Cannot resolve symbol: value" (Spring Boot's PropertyMapper.Source). */
public class LambdaBlockLocalTypeVariableTest<T> {
    final T held;

    LambdaBlockLocalTypeVariableTest(T held) {
        this.held = held;
    }

    T getValue() {
        return held;
    }

    Supplier<T> orElse(Supplier<? extends T> fallback) {
        Supplier<T> supplier = () -> {
            T value = getValue();
            return (value != null) ? value : fallback.get();
        };
        return supplier;
    }

    <R> Supplier<R> map(Function<? super T, ? extends R> f) {
        Supplier<R> supplier = () -> {
            T value = getValue();
            return (value != null) ? f.apply(value) : null;
        };
        return supplier;
    }

    public static void main(String[] args) {
        LambdaBlockLocalTypeVariableTest<String> some = new LambdaBlockLocalTypeVariableTest<>("a");
        LambdaBlockLocalTypeVariableTest<String> none = new LambdaBlockLocalTypeVariableTest<>(null);
        if (!"a".equals(some.orElse(() -> "b").get()) || !"b".equals(none.orElse(() -> "b").get())
                || !"A".equals(some.map(String::toUpperCase).get()) || none.map(String::length).get() != null) {
            throw new RuntimeException("block local");
        }
        System.out.println("LambdaBlockLocalTypeVariableTest passed!");
    }
}
