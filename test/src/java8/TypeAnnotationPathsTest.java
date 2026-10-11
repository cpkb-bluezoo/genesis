import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.reflect.AnnotatedArrayType;
import java.lang.reflect.AnnotatedParameterizedType;
import java.lang.reflect.AnnotatedType;
import java.lang.reflect.AnnotatedWildcardType;
import java.lang.reflect.Method;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

/*
 * Regression test: a type annotation on a type argument ("Function<@Marker T, ?>") was
 * counted into RuntimeVisibleTypeAnnotations but never written, leaving an attribute with a
 * count and no entries: the class could not be read by javap, and its reflection of annotated
 * types was wrong. The annotation also belongs at the right type_path (type argument, wildcard
 * bound, array component).
 */
public class TypeAnnotationPathsTest {

    @Target(ElementType.TYPE_USE)
    @Retention(RetentionPolicy.RUNTIME)
    @interface Marker {
    }

    List<@Marker String> field;

    public <T> Function<T, ?> withArgument(Function<@Marker T, ?> extractor, Map<String, @Marker Integer> map) {
        return null;
    }

    public List<? extends @Marker Number> wildcardReturn() {
        return null;
    }

    public List<@Marker String>[] elementOfArray() {
        return null;
    }

    public String @Marker [] arrayItself() {
        return null;
    }

    static boolean marked(AnnotatedType type) {
        return type.getAnnotation(Marker.class) != null;
    }

    static Method method(String name) throws Exception {
        for (Method m : TypeAnnotationPathsTest.class.getDeclaredMethods()) {
            if (m.getName().equals(name)) {
                return m;
            }
        }
        throw new AssertionError(name);
    }

    static void check(boolean condition, String what) {
        if (!condition) {
            throw new RuntimeException("FAIL: " + what);
        }
    }

    public static void main(String[] args) throws Exception {
        AnnotatedParameterizedType fieldType = (AnnotatedParameterizedType)
            TypeAnnotationPathsTest.class.getDeclaredField("field").getAnnotatedType();
        check(!marked(fieldType) && marked(fieldType.getAnnotatedActualTypeArguments()[0]), "field type argument");

        Method with = method("withArgument");
        AnnotatedType[] params = with.getAnnotatedParameterTypes();
        AnnotatedParameterizedType first = (AnnotatedParameterizedType) params[0];
        check(!marked(first) && marked(first.getAnnotatedActualTypeArguments()[0])
              && !marked(first.getAnnotatedActualTypeArguments()[1]), "first parameter type arguments");
        AnnotatedParameterizedType second = (AnnotatedParameterizedType) params[1];
        check(!marked(second.getAnnotatedActualTypeArguments()[0])
              && marked(second.getAnnotatedActualTypeArguments()[1]), "second parameter type arguments");

        AnnotatedParameterizedType wildcard = (AnnotatedParameterizedType) method("wildcardReturn").getAnnotatedReturnType();
        AnnotatedWildcardType wild = (AnnotatedWildcardType) wildcard.getAnnotatedActualTypeArguments()[0];
        check(marked(wild.getAnnotatedUpperBounds()[0]), "wildcard bound");

        AnnotatedArrayType element = (AnnotatedArrayType) method("elementOfArray").getAnnotatedReturnType();
        AnnotatedParameterizedType component = (AnnotatedParameterizedType) element.getAnnotatedGenericComponentType();
        check(!marked(element) && !marked(component) && marked(component.getAnnotatedActualTypeArguments()[0]),
              "array element");

        AnnotatedArrayType itself = (AnnotatedArrayType) method("arrayItself").getAnnotatedReturnType();
        check(marked(itself) && !marked(itself.getAnnotatedGenericComponentType()), "array itself");

        System.out.println("TypeAnnotationPathsTest passed!");
    }
}
