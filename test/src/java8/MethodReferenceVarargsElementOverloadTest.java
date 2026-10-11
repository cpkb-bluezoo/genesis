import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

/*
 * Regression test: "source.to(builder::intercept)" with the overloads intercept(Item...) and
 * intercept(List<Item>), for a Consumer<? super Sub>, took the List one (BootstrapMethodError:
 * "Sub is not convertible to List"): a wildcard argument is its bound, and the one argument at
 * a varargs parameter's position is an element of it.
 */
public class MethodReferenceVarargsElementOverloadTest {

    interface Item {
    }

    static class Sub implements Item {
        Sub(Long deadline) {
        }
    }

    static class Builder {
        String result = "";

        Builder intercept(Item... items) {
            result += "varargs";
            return this;
        }

        Builder intercept(List<Item> items) {
            result += "list";
            return this;
        }
    }

    static class Source<T> {
        final T value;

        Source(T value) {
            this.value = value;
        }

        <R> Source<R> as(Function<? super T, ? extends R> function) {
            return new Source<R>(function.apply(this.value));
        }

        void to(Consumer<? super T> consumer) {
            consumer.accept(this.value);
        }
    }

    static <T> Source<T> from(Supplier<T> supplier) {
        return new Source<T>(supplier.get());
    }

    public static void main(String[] args) {
        Builder builder = new Builder();
        from(() -> 5L).as(Sub::new).to(builder::intercept);
        Source<Sub> source = from(() -> 6L).as(Sub::new);
        source.to(builder::intercept);
        if (!"varargsvarargs".equals(builder.result)) {
            throw new RuntimeException("FAIL: " + builder.result);
        }
        System.out.println("PASS");
    }
}
