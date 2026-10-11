import java.util.function.Supplier;

/* Regression test: "Type::new" for a class that declares no constructor - it has
 * the default one, which no symbol stands for ("Cannot find constructor for
 * ColorConverter::new with 0 parameter(s)" in Spring Boot). */
class DefaultConstructed {
}

public class DefaultConstructorReferenceTest {
    static class Nested {
        String name() {
            return "nested";
        }
    }

    static <T> T make(Class<T> type, Supplier<T> supplier) {
        return supplier.get();
    }

    public static void main(String[] args) {
        Supplier<Nested> s = Nested::new;
        Nested n = make(Nested.class, Nested::new);
        DefaultConstructed d = make(DefaultConstructed.class, DefaultConstructed::new);
        if (!"nested".equals(s.get().name()) || !"nested".equals(n.name()) || d == null) {
            throw new RuntimeException("default constructor reference");
        }
        System.out.println("DefaultConstructorReferenceTest passed!");
    }
}
