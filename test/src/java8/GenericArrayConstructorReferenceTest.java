import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/* Regression test: a method reference to the constructor of an array of a
 * parameterized type (Class<?>[]::new) as an argument in a return statement
 * failed to parse: the "<" was taken for a comparison ("Expected ')', found
 * '?'") - Spring Boot's BindableRuntimeHintsRegistrar. */
public class GenericArrayConstructorReferenceTest {
    static int count(Class<?>[] types) {
        return types.length;
    }

    static int viaReturn(List<Class<?>> l) {
        return count(l.stream().toArray(Class<?>[]::new));
    }

    static int lists(List<List<?>> l) {
        return l.stream().toArray(List<?>[]::new).length;
    }

    public static void main(String[] args) {
        Class<?>[] arr = Stream.of(String.class, Integer.class).toArray(Class<?>[]::new);
        if (viaReturn(List.of(String.class, Integer.class, Long.class)) != 3 || arr.length != 2
                || lists(List.of(List.of(), List.of(1))) != 2
                || Stream.of("a").collect(Collectors.toList()).size() != 1) {
            throw new RuntimeException("generic array constructor reference");
        }
        System.out.println("GenericArrayConstructorReferenceTest passed!");
    }
}
