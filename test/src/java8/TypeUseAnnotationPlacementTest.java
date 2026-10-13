import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.reflect.Field;
import java.lang.reflect.Method;

/* Regression test: an annotation whose @Target is TYPE_USE only, written in
 * front of a field, method or parameter ("@Nullable String f"), annotates the
 * type, not the declaration. genesis emitted it as a declaration annotation, so
 * reflection saw it on the field, method and parameter (Spring printed
 * "@Nullable Class<?>" in a conversion error message). */
public class TypeUseAnnotationPlacementTest {
    @Retention(RetentionPolicy.RUNTIME)
    @Target(ElementType.TYPE_USE)
    @interface TypeOnly {
    }

    @Retention(RetentionPolicy.RUNTIME)
    @Target({ElementType.TYPE_USE, ElementType.FIELD, ElementType.METHOD, ElementType.PARAMETER})
    @interface Both {
    }

    private @TypeOnly Class<?> type;
    private @Both String both;
    private @TypeOnly String[] array;

    @TypeOnly String get() {
        return null;
    }

    void set(@TypeOnly Class<?> value, @Both String other) {
    }

    private static void check(boolean ok, String what) {
        if (!ok) {
            System.out.println("FAILED: " + what);
            System.exit(1);
        }
    }

    public static void main(String[] args) throws Exception {
        Class<?> c = TypeUseAnnotationPlacementTest.class;
        Field type = c.getDeclaredField("type");
        check(type.getAnnotations().length == 0, "TYPE_USE-only is no field annotation");
        check(type.getAnnotatedType().getAnnotations().length == 1, "TYPE_USE-only is a field type annotation");
        Field both = c.getDeclaredField("both");
        check(both.getAnnotations().length == 1, "TYPE_USE and FIELD is a field annotation");
        Field array = c.getDeclaredField("array");
        check(array.getAnnotations().length == 0, "TYPE_USE-only array is no field annotation");

        Method get = c.getDeclaredMethod("get");
        check(get.getAnnotations().length == 0, "TYPE_USE-only is no method annotation");
        check(get.getAnnotatedReturnType().getAnnotations().length == 1, "TYPE_USE-only is a return type annotation");

        Method set = c.getDeclaredMethod("set", Class.class, String.class);
        check(set.getParameterAnnotations()[0].length == 0, "TYPE_USE-only is no parameter annotation");
        check(set.getParameters()[0].getAnnotatedType().getAnnotations().length == 1,
              "TYPE_USE-only is a parameter type annotation");
        check(set.getParameterAnnotations()[1].length == 1, "TYPE_USE and PARAMETER is a parameter annotation");
        System.out.println("TypeUseAnnotationPlacementTest passed!");
    }
}
