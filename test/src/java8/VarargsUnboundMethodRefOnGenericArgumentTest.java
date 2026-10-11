import java.util.function.BiConsumer;

/*
 * Regression test: "Builder::intercept" (a varargs instance method, written
 * out as a lambda) passed to a generic method whose own type variable is the
 * only thing that could name the first parameter got a lambda whose
 * parameters were Object: the call "$mref0.intercept(...)" was compiled
 * against Object with a made-up "(I)I" descriptor and failed verification.
 */
public class VarargsUnboundMethodRefOnGenericArgumentTest {

    interface Interceptor {
    }

    interface OldInterceptor {
    }

    interface Builder<T> {
        Builder<T> intercept(Interceptor... interceptors);

        Builder<T> intercept(OldInterceptor... interceptors);
    }

    static int calls;

    static <T, I> void callsWith(BiConsumer<T, I> action, I argument, T target) {
        action.accept(target, argument);
    }

    static <T> void run(Interceptor interceptor, Builder<T> builder) {
        callsWith(Builder::intercept, interceptor, builder);
    }

    public static void main(String[] args) {
        Builder<String> builder = new Builder<String>() {
            public Builder<String> intercept(Interceptor... interceptors) {
                calls += interceptors.length;
                return this;
            }

            public Builder<String> intercept(OldInterceptor... interceptors) {
                calls += 100;
                return this;
            }
        };
        run(new Interceptor() { }, builder);
        if (calls != 1) {
            throw new RuntimeException("FAIL: expected 1 got " + calls);
        }
        System.out.println("VarargsUnboundMethodRefOnGenericArgumentTest passed!");
    }
}
