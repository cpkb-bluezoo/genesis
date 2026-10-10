import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/* Regression test: an annotation element given by the name of a constant
 * (@Opts(timeout = WAIT_MS)) rather than a literal. The name was written to
 * the class file as a string, so reading the element reflectively failed with
 * AnnotationTypeMismatchException "Found data of type String[WAIT_MS]" (JUnit's
 * @Test(timeout = WAIT_MS) in gumdrop's ServletResponseBackpressureTest). */
public class AnnotationNamedConstantTest {
    @Retention(RetentionPolicy.RUNTIME)
    @interface Opts {
        long timeout() default 0L;
        int count() default 0;
        String name() default "";
        boolean flag() default false;
        double ratio() default 0.0;
    }

    private static final long WAIT_MS = 20000L;
    static final int COUNT = 3;
    static final String NAME = "alpha";
    static final boolean FLAG = true;
    static final double RATIO = 0.5;
    static final long NEG = -7L;
    static final int BASE = 40;
    static final int SUM = BASE + 2;

    static class Holder {
        static final long OTHER = 99L;
    }

    @Opts(timeout = WAIT_MS, count = COUNT, name = NAME, flag = FLAG, ratio = RATIO)
    public void one() { }

    @Opts(timeout = NEG, count = SUM)
    public void two() { }

    @Opts(timeout = Holder.OTHER, name = AnnotationNamedConstantTest.NAME)
    public void three() { }

    public static void main(String[] args) throws Exception {
        Opts a = AnnotationNamedConstantTest.class.getMethod("one").getAnnotation(Opts.class);
        if (a.timeout() != 20000L || a.count() != 3 || !"alpha".equals(a.name())
                || !a.flag() || a.ratio() != 0.5) {
            throw new RuntimeException("one: " + a.timeout() + " " + a.count() + " " + a.name()
                + " " + a.flag() + " " + a.ratio());
        }
        Opts b = AnnotationNamedConstantTest.class.getMethod("two").getAnnotation(Opts.class);
        if (b.timeout() != -7L || b.count() != 42) {
            throw new RuntimeException("two: " + b.timeout() + " " + b.count());
        }
        Opts c = AnnotationNamedConstantTest.class.getMethod("three").getAnnotation(Opts.class);
        if (c.timeout() != 99L || !"alpha".equals(c.name())) {
            throw new RuntimeException("three: " + c.timeout() + " " + c.name());
        }
        System.out.println("AnnotationNamedConstantTest passed!");
    }
}
