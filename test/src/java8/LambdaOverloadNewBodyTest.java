import java.util.function.Supplier;

public class LambdaOverloadNewBodyTest {
    static String chosen;

    static final class ResourceId {
        private final String id;
        ResourceId(String id) { this.id = id; }
    }

    // a functional interface of its own
    interface Resource {
        ResourceId getId();
    }

    static final class ExampleResource implements Resource {
        public ResourceId getId() { return new ResourceId("test"); }
    }

    static final class ResourceProvider {
        ResourceProvider(Resource resource) { chosen = "resource"; }
        ResourceProvider(Supplier<Resource> supplier) { chosen = "supplier"; }
    }

    static final class Box<T> {
        static <T> Box<T> of() { return new Box<>(); }
    }

    static final class Holder {
        Holder(Resource resource) { chosen = "resource"; }
        Holder(Supplier<Box<String>> supplier) { chosen = "supplier"; }
    }

    // A lambda with no parameters is explicitly typed, so its body takes part in applicability
    // (JLS 15.12.2.2): `new ExampleResource()` is no ResourceId, so Resource does not apply.
    public static void main(String[] args) {
        new ResourceProvider(() -> new ExampleResource());
        if (!"supplier".equals(chosen)) throw new AssertionError(chosen);
        // a diamond creation adapts to its target, so it says nothing against Supplier<Box<String>>
        new Holder(() -> new Box<>());
        if (!"supplier".equals(chosen)) throw new AssertionError(chosen);
        System.out.println("ok");
    }
}
