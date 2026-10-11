import java.util.function.Function;
import java.util.function.Supplier;

/* Regression test: lambdas and method references passed to a varargs parameter
 * of a functional interface type (Spring Boot's ChangelogWriter.getFirstNonNull
 * (T, Function<T, P>...)): each is an element, and the type variable they leave
 * is inferred from them. */
public class VarargsFunctionalArgumentsTest {
    static class D {
        String a() {
            return null;
        }

        String b() {
            return "b";
        }
    }

    @SafeVarargs
    static <T, P> P firstNonNull(T t, Function<T, P>... fs) {
        for (Function<T, P> f : fs) {
            P p = f.apply(t);
            if (p != null) {
                return p;
            }
        }
        return null;
    }

    static String any(Supplier<String>... ss) {
        return ss.length == 0 ? "none" : ss[ss.length - 1].get();
    }

    public static void main(String[] args) {
        D d = new D();
        String viaRefs = firstNonNull(d, D::a, D::b);
        String viaLambdas = firstNonNull(d, (x) -> x.a(), (x) -> "l");
        if (!"b".equals(viaRefs) || !"l".equals(viaLambdas) || !"x".equals(any(() -> "w", () -> "x"))
                || !"none".equals(any())) {
            throw new RuntimeException(viaRefs + viaLambdas);
        }
        System.out.println("VarargsFunctionalArgumentsTest passed!");
    }
}
