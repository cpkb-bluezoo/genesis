import java.util.function.Supplier;

/**
 * An anonymous class created inside a lambda that sits in an instance field initializer: the
 * lambda has to capture "this" so the anonymous class gets its enclosing instance.
 */
public class AnonymousInLambdaInFieldInitTest {

    interface Config {
        String name();
    }

    private final String prefix = "cfg-";

    private final Supplier<Config> factory = () -> new Config() {
        @Override
        public String name() {
            return prefix + "a";
        }
    };

    private final Supplier<Supplier<Config>> nested = () -> () -> new Config() {
        @Override
        public String name() {
            return prefix + "b";
        }
    };

    private final Supplier<Config> plain = () -> new Config() {
        @Override
        public String name() {
            return "plain";
        }
    };

    private static final Supplier<Config> STATIC_FACTORY = () -> new Config() {
        @Override
        public String name() {
            return "static";
        }
    };

    public static void main(String[] args) {
        AnonymousInLambdaInFieldInitTest t = new AnonymousInLambdaInFieldInitTest();
        if (!"cfg-a".equals(t.factory.get().name())) {
            throw new AssertionError("a: " + t.factory.get().name());
        }
        if (!"cfg-b".equals(t.nested.get().get().name())) {
            throw new AssertionError("b: " + t.nested.get().get().name());
        }
        if (!"plain".equals(t.plain.get().name()) || !"static".equals(STATIC_FACTORY.get().name())) {
            throw new AssertionError("plain/static");
        }
        System.out.println("PASS");
    }
}
