import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.function.Function;

/* Regression test: type-use annotations (JSR 308; JSpecify's @Nullable is
 * everywhere in Spring Boot) in the positions that failed to parse: after a
 * method's type parameters, as the first type argument of a parameter, in a
 * cast, on a for or enhanced-for variable, on a lambda parameter, and on a
 * nested type named through its outer one (Outer.@Nullable Inner), and on a
 * wildcard (Consumer<@Nullable ? super T>). */
public class TypeUseAnnotationPositionsTest {
    @Target({ElementType.TYPE_USE, ElementType.TYPE_PARAMETER})
    @Retention(RetentionPolicy.RUNTIME)
    @interface Nullable {
    }

    // after the type parameters
    static <K> @Nullable Object afterTypeParameters(K key, boolean none) {
        return none ? null : key;
    }

    // a leading annotated type argument, in a method and a constructor parameter
    final Callable<@Nullable Object> callable;

    TypeUseAnnotationPositionsTest(Callable<@Nullable Object> callable) {
        this.callable = callable;
    }

    static String typeArgument(Callable<@Nullable Object> c, Map<@Nullable String, Object> m) throws Exception {
        return String.valueOf(c.call()) + m.size();
    }

    // an annotation inside a qualified type name, in a field, a constructor and a method parameter
    static class Outer {
        static class Inner {
        }
    }

    Outer.@Nullable Inner inner;

    TypeUseAnnotationPositionsTest(int unused, Outer.@Nullable Inner inner) {
        this.inner = inner;
        this.callable = null;
    }

    static int nested(List<Outer.@Nullable Inner> list, java.util.@Nullable List<String> other) {
        return list.size() + (other == null ? 0 : other.size());
    }

    // an annotated wildcard
    static void wildcard(java.util.function.Consumer<@Nullable ? super String> c, List<@Nullable ?> any) {
        c.accept("w" + any.size());
    }

    // a cast to an annotated type
    static String cast(Object o) {
        return (@Nullable String) o;
    }

    // annotated loop variables
    static int loops(List<String> items) {
        int n = 0;
        for (@Nullable String s : items) {
            n += s == null ? 100 : s.length();
        }
        for (final @Nullable String t : items) {
            n += t == null ? 100 : 1;
        }
        for (@Nullable String u = items.get(0); u != null && n < 1000; u = null) {
            n += 1000;
        }
        return n;
    }

    // an annotated lambda parameter
    static String lambda(String in) {
        Function<String, String> f = (@Nullable String s) -> s + "!";
        return f.apply(in);
    }

    public static void main(String[] args) throws Exception {
        check(afterTypeParameters("k", false), "k");
        check(afterTypeParameters("k", true), null);
        check(typeArgument(() -> null, Map.of("a", 1)), "null1");
        check(new TypeUseAnnotationPositionsTest(() -> "c").callable.call(), "c");
        check(cast("abc"), "abc");
        check(loops(new ArrayList<>(Arrays.asList("ab", "c"))), 1000 + 3 + 2);
        check(lambda("x"), "x!");
        StringBuilder wild = new StringBuilder();
        wildcard((s) -> wild.append(s), List.of());
        check(wild.toString(), "w0");
        check(nested(List.of(new Outer.Inner()), null), 1);
        check(new TypeUseAnnotationPositionsTest(1, new Outer.Inner()).inner != null, true);
        System.out.println("TypeUseAnnotationPositionsTest passed!");
    }

    static void check(Object got, Object want) {
        if (want == null ? got != null : !want.equals(got)) {
            throw new RuntimeException("got " + got + ", want " + want);
        }
    }
}
