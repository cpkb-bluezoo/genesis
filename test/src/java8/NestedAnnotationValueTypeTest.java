import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/*
 * Regression test: an annotation nested in an annotation value
 * ("@Deep(level1 = @Deep.Level1(...))") wrote the nested annotation's type
 * descriptor with '/' at every nesting boundary ("LOuter/Deep/Level1;")
 * instead of '$', so reflection threw TypeNotPresentException on
 * getAnnotation().
 */
public class NestedAnnotationValueTypeTest {

    @Retention(RetentionPolicy.RUNTIME)
    @interface Deep {
        Level1 level1();

        @Retention(RetentionPolicy.RUNTIME)
        @interface Level1 {
            Level2 level2();
        }

        @Retention(RetentionPolicy.RUNTIME)
        @interface Level2 {
            String value();
        }
    }

    @Deep(level1 = @NestedAnnotationValueTypeTest.Deep.Level1(
            level2 = @NestedAnnotationValueTypeTest.Deep.Level2("l2")))
    static class Holder {
    }

    public static void main(String[] args) {
        Deep d = Holder.class.getAnnotation(Deep.class);
        String actual = d.level1().level2().value();
        if (!"l2".equals(actual)) {
            throw new RuntimeException("FAIL: expected l2 got " + actual);
        }
        System.out.println("NestedAnnotationValueTypeTest passed!");
    }
}
