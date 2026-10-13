import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.reflect.Method;
import java.util.Arrays;

/* Regression test: an annotation on a record component goes to the members
 * derived from it that its @Target allows (JLS 8.10.3): the field, the
 * accessor, the canonical constructor's parameter and the record component.
 * genesis put every one of them on the field only, so Spring Boot's
 * @Name("import") on a record component never reached the constructor
 * parameter the binder reads. */
public class RecordComponentAnnotationPropagationTest {
    @Retention(RetentionPolicy.RUNTIME)
    @Target({ElementType.FIELD, ElementType.PARAMETER})
    @interface OnFieldAndParameter {
        String value();
    }

    @Retention(RetentionPolicy.RUNTIME)
    @Target({ElementType.METHOD, ElementType.RECORD_COMPONENT})
    @interface OnAccessor {
    }

    @Retention(RetentionPolicy.RUNTIME)
    @interface Anywhere {
    }

    @Retention(RetentionPolicy.RUNTIME)
    @Target(ElementType.TYPE_USE)
    @interface OnType {
    }

    record Implicit(@OnFieldAndParameter("import") @OnAccessor @Anywhere String name, @OnType String typed, int x) {
    }

    record Compact(@OnFieldAndParameter("c") String name) {
        Compact {
            if (name == null) {
                name = "";
            }
        }
    }

    record Explicit(@OnFieldAndParameter("e") String name) {
        Explicit(String name) {
            this.name = name;
        }
    }

    private static String names(java.lang.annotation.Annotation[] annotations) {
        String[] out = new String[annotations.length];
        for (int i = 0; i < out.length; i++) {
            out[i] = annotations[i].annotationType().getSimpleName();
        }
        Arrays.sort(out);
        return Arrays.toString(out);
    }

    private static void check(String expected, String actual, String what) {
        if (!expected.equals(actual)) {
            System.out.println("FAILED " + what + ": expected " + expected + ", got " + actual);
            System.exit(1);
        }
    }

    public static void main(String[] args) throws Exception {
        Class<?> c = Implicit.class;
        check("[Anywhere, OnFieldAndParameter]", names(c.getDeclaredField("name").getAnnotations()), "field");
        check("[Anywhere, OnAccessor]", names(c.getDeclaredMethod("name").getAnnotations()), "accessor");
        check("[Anywhere, OnAccessor]", names(c.getRecordComponents()[0].getAnnotations()), "component");
        check("[Anywhere, OnFieldAndParameter]",
              names(c.getDeclaredConstructors()[0].getParameterAnnotations()[0]), "constructor parameter");
        check("[]", names(c.getDeclaredField("typed").getAnnotations()), "TYPE_USE-only field");
        check("[]", names(c.getDeclaredMethod("typed").getAnnotations()), "TYPE_USE-only accessor");
        check("[]", names(c.getDeclaredConstructors()[0].getParameterAnnotations()[1]), "TYPE_USE-only parameter");
        check("[OnType]", names(c.getDeclaredField("typed").getAnnotatedType().getAnnotations()), "TYPE_USE-only field type");

        check("[OnFieldAndParameter]",
              names(Compact.class.getDeclaredConstructors()[0].getParameterAnnotations()[0]), "compact constructor parameter");
        check("[]", names(Explicit.class.getDeclaredConstructors()[0].getParameterAnnotations()[0]),
              "explicit constructor parameter");
        System.out.println("RecordComponentAnnotationPropagationTest passed!");
    }
}
