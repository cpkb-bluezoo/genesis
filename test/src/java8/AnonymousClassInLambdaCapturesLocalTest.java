import java.util.function.Supplier;

/* Regression test: an anonymous class created in a lambda captures the locals its
 * body uses, so the lambda must capture them too (to hand them to the class's
 * constructor): "() -> new L() { ... c.getSimpleName() ... }" had no c to pass
 * and failed verification. */
public class AnonymousClassInLambdaCapturesLocalTest {
    interface Named {
        String name();
    }

    static Named make(Class<?> c, String prefix) {
        Supplier<Named> s = () -> new Named() {
            @Override
            public String name() {
                return prefix + c.getSimpleName();
            }
        };
        return s.get();
    }

    Named viaInstance(String suffix) {
        Supplier<Named> s = () -> new Named() {
            @Override
            public String name() {
                return suffix + "!";
            }
        };
        return s.get();
    }

    public static void main(String[] args) {
        if (!"my-String".equals(make(String.class, "my-").name())
                || !"x!".equals(new AnonymousClassInLambdaCapturesLocalTest().viaInstance("x").name())) {
            throw new RuntimeException("anonymous in lambda");
        }
        System.out.println("AnonymousClassInLambdaCapturesLocalTest passed!");
    }
}
