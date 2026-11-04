import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/*
 * Two things a stream pipeline of method references needs:
 *  - "Outcome::<String, Exception>ok" names the instance method ok(), not the
 *    static ok(T): the type arguments written leave the static one no argument
 *    it could take (JLS 15.13.1);
 *  - "flatMap(Optional::get)" is accepted for a Function whose result is a
 *    "? extends Stream<? extends R>", the result of get() being the Optional's
 *    own variable, which the receiver fixes.
 */
public class MethodRefExplicitTypeArgsFlatMapTest {
    interface Outcome<T, E> {
        Optional<T> ok();

        static <T, E> Outcome<T, E> ok(T value) {
            return () -> Optional.ofNullable(value);
        }
    }

    static Outcome<Stream<String>, Exception> lines(String text) {
        return () -> Optional.of(Arrays.stream(text.split(",")));
    }

    public static void main(String[] args) {
        List<String> got = Stream.of("a,b", "c")
                .map(MethodRefExplicitTypeArgsFlatMapTest::lines)
                .map(Outcome::<Stream<String>, Exception>ok)
                .filter(Optional::isPresent)
                .flatMap(Optional::get)
                .collect(Collectors.toList());
        if (!got.equals(Arrays.asList("a", "b", "c"))) {
            throw new AssertionError(got);
        }

        List<Supplier<Stream<Integer>>> suppliers = new ArrayList<>();
        suppliers.add(() -> Stream.of(1, 2));
        suppliers.add(() -> Stream.of(3));
        int sum = suppliers.stream().flatMap(Supplier::get).mapToInt(i -> i).sum();
        if (sum != 6) {
            throw new AssertionError(sum);
        }
        System.out.println("MethodRefExplicitTypeArgsFlatMapTest passed!");
    }
}
