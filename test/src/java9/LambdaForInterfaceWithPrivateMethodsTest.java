import java.util.function.Predicate;

/* Regression test: a functional interface that also declares private instance
 * methods (Java 9) - they were counted as abstract methods, so the interface
 * had no single abstract method and a lambda for it resolved nothing (Spring
 * Boot's JsonWriter.Members.ValueExtractor). */
@FunctionalInterface
interface PrivateHelpers<T> {
    Object extract(Object instance);

    default PrivateHelpers<T> when(Predicate<Object> predicate) {
        return (instance) -> test(extract(instance), predicate);
    }

    private Object test(Object extracted, Predicate<Object> predicate) {
        return predicate.test(extracted) ? extracted : "skipped";
    }
}

public class LambdaForInterfaceWithPrivateMethodsTest {
    public static void main(String[] args) {
        PrivateHelpers<String> p = (instance) -> instance;
        if (!"x".equals(p.when(o -> true).extract("x")) || !"skipped".equals(p.when(o -> false).extract("x"))) {
            throw new RuntimeException("when");
        }
        System.out.println("LambdaForInterfaceWithPrivateMethodsTest passed!");
    }
}
