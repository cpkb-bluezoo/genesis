import java.util.function.Consumer;

/**
 * Regression: bound method ref to a varargs method (builder::intercept) used as
 * Consumer must call the varargs overload with one element, not package wrongly
 * for Mockito/PropertyMapper (.to(builder::intercept)).
 */
public class BoundVarargsMethodRefConsumerVerifyTest {

    static class Interceptor {
    }

    static class Sub extends Interceptor {
    }

    static class Builder {
        Object lastInterceptArg;

        Builder intercept(Interceptor... interceptors) {
            this.lastInterceptArg = interceptors;
            return this;
        }
    }

    public static void main(String[] args) {
        Builder builder = new Builder();
        Consumer<Sub> consumer = builder::intercept;
        consumer.accept(new Sub());
        if (!(builder.lastInterceptArg instanceof Interceptor[])) {
            throw new RuntimeException("expected varargs array, got " + builder.lastInterceptArg);
        }
        Interceptor[] arr = (Interceptor[]) builder.lastInterceptArg;
        if (arr.length != 1 || !(arr[0] instanceof Sub)) {
            throw new RuntimeException("bad array contents");
        }
        System.out.println("PASS");
    }
}
