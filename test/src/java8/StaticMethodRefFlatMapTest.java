import java.lang.reflect.Method;
import java.util.Collection;
import java.util.List;
import java.util.stream.Stream;

/* Regression test: ".map(Method::getParameterTypes).flatMap(Stream::of)" - the
 * static Stream.of is the T... overload (more specific than of(T)) and its own T
 * is fixed by the Class<?>[] it is given, so the flattened stream is of Class
 * (Spring Boot's SpringBootJoranConfigurator.parameterTypesNames). */
public class StaticMethodRefFlatMapTest {
    static Collection<Class<?>> parameterTypes(Collection<Method> methods) {
        return methods.stream()
            .map(Method::getParameterTypes)
            .flatMap(Stream::of)
            .filter((type) -> !type.isPrimitive() && !type.equals(Integer.class))
            .map((type) -> type.isArray() ? type.getComponentType() : type)
            .toList();
    }

    public static void main(String[] args) throws Exception {
        Collection<Class<?>> types = parameterTypes(List.of(String.class.getMethod("concat", String.class),
                String.class.getMethod("indexOf", String.class, int.class),
                String.class.getMethod("getChars", int.class, int.class, char[].class, int.class)));
        if (types.size() != 3 || !types.contains(String.class) || !types.contains(char.class)) {
            throw new RuntimeException(types.toString());
        }
        System.out.println("StaticMethodRefFlatMapTest passed!");
    }
}
