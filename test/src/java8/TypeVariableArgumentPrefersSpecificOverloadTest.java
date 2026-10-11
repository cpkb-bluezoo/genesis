/*
 * Regression test: an argument whose type is a type variable of the caller
 * ("<A extends Provider<String>> ... check(a)") was only ever matched by the overload taking a
 * bare type variable ("<T> Plain<T> check(T)"), never by the more specific "<T> T check(Provider<T>)",
 * as AssertJ's assertThat(AssertProvider<T>) vs assertThat(T) show: the result was then typed as
 * the wrong thing and the call on it was emitted against the wrong class (VerifyError).
 */
public class TypeVariableArgumentPrefersSpecificOverloadTest {

    interface Provider<T> {
        T get();
    }

    static class Plain<T> {
        final T value;

        Plain(T value) {
            this.value = value;
        }
    }

    static class Special {
        String hello() {
            return "special";
        }
    }

    static <T> Plain<T> check(T value) {
        return new Plain<T>(value);
    }

    static <T> T check(Provider<T> provider) {
        return provider.get();
    }

    interface SpecialProvider<X extends Number> extends Provider<Special> {
    }

    static <X extends Number, A extends SpecialProvider<X>> String viaTypeVariable(A provider) {
        return check(provider).hello();
    }

    static <A extends SpecialProvider<Integer>> String viaClosedBound(A provider) {
        return check(provider).hello();
    }

    public static void main(String[] args) {
        SpecialProvider<Integer> provider = new SpecialProvider<Integer>() {
            @Override
            public Special get() {
                return new Special();
            }
        };
        String actual = viaTypeVariable(provider) + viaClosedBound(provider);
        if (!"specialspecial".equals(actual)) {
            throw new RuntimeException("FAIL: " + actual);
        }
        System.out.println("PASS");
    }
}
