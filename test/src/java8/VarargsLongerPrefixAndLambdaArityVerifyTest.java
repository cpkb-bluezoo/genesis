/**
 * Two varargs regressions from ranking equal-scoring varargs methods by
 * candidate order.
 *
 * super(Class, Class, Class, Class) must be (Class, Class, Class...), not
 * (Class, Class...). registerBean(Class, () -> value) must be the Supplier
 * overload: a zero-argument lambda is not a one-argument customizer, and
 * binding it as one writes a method whose descriptor does not fit its locals.
 */
public class VarargsLongerPrefixAndLambdaArityVerifyTest {

    interface Customizer {
        void customize(Object bean);
    }

    interface Supply<T> {
        T get();
    }

    static class Ctx {
        String which;

        public <T> void registerBean(Class<T> type, Supply<T> supplier, Customizer... customizers) {
            which = "supplier:" + supplier.get();
        }

        public <T> void registerBean(Class<T> type, Customizer... customizers) {
            which = "customizer";
        }
    }

    static class Post {
        String which;

        Post(Class<?> beanClass, Class<?> factoryBeanClass, Class<?>... dependencyTypes) {
            which = "three:" + dependencyTypes.length;
        }

        Post(Class<?> beanClass, Class<?>... dependencyTypes) {
            which = "two:" + dependencyTypes.length;
        }
    }

    public static void main(String[] args) {
        String value = "path";
        Ctx ctx = new Ctx();
        ctx.registerBean(String.class, () -> value);
        if (!"supplier:path".equals(ctx.which)) {
            throw new RuntimeException("registerBean got " + ctx.which);
        }
        Post post = new Post(String.class, Integer.class, Long.class, Double.class);
        if (!"three:2".equals(post.which)) {
            throw new RuntimeException("constructor got " + post.which);
        }
        System.out.println("PASS");
    }
}
