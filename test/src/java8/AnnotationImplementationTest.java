import java.lang.annotation.Annotation;

@interface Tag {
    String value() default "";
}

public class AnnotationImplementationTest {
    static class ManualTag implements Tag {
        @Override
        public Class<? extends Annotation> annotationType() {
            return Tag.class;
        }

        @Override
        public String value() {
            return "manual";
        }

        @Override
        public boolean equals(Object o) {
            return o instanceof Tag;
        }

        @Override
        public int hashCode() {
            return 7;
        }

        @Override
        public String toString() {
            return "@Tag";
        }
    }

    public static void main(String[] args) {
        Tag t = new ManualTag();
        if (t.annotationType() != Tag.class) throw new AssertionError("type");
        Annotation a = t;
        if (a.annotationType() != Tag.class || !"manual".equals(t.value())) throw new AssertionError("via Annotation");
        System.out.println("ok");
    }
}
