import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.function.BiFunction;
import java.util.function.Function;

/*
 * Annotations and "final" on lambda parameters, in every order, with and
 * without "var" (Java 11).  An annotation followed by "final" was rejected
 * with "Expected type".
 */
public class VarLambdaAnnotationTest {

    @Retention(RetentionPolicy.RUNTIME)
    @Target({ElementType.PARAMETER, ElementType.TYPE_USE})
    @interface NonNull {}

    @Retention(RetentionPolicy.RUNTIME)
    @Target(ElementType.PARAMETER)
    @interface Named { String value(); }

    static void check(boolean ok, String what) {
        if (!ok) {
            System.out.println("FAILED: " + what);
            System.exit(1);
        }
    }

    public static void main(String[] args) {
        Function<String, String> a = (@NonNull var x) -> x + "!";
        check("a!".equals(a.apply("a")), "annotated var");

        Function<String, String> b = (@NonNull final var x) -> x + "?";
        check("b?".equals(b.apply("b")), "annotation then final, var");

        Function<String, String> c = (final @NonNull var x) -> x + "#";
        check("c#".equals(c.apply("c")), "final then annotation, var");

        Function<String, String> d = (@Named("n") final String x) -> x + "$";
        check("d$".equals(d.apply("d")), "annotation with a value then final, typed");

        BiFunction<String, String, String> e =
            (@NonNull var x, @Named("y") final var y) -> x + y;
        check("ef".equals(e.apply("e", "f")), "two parameters, mixed");

        BiFunction<Integer, Integer, Integer> f =
            (@NonNull @Named("a") final var x, final @NonNull var y) -> x + y;
        check(f.apply(3, 4) == 7, "two annotations then final");
    }
}
