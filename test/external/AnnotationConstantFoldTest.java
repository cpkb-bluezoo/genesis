import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

import annconst.Precedence;

/**
 * Precedence.ORDER is a class file constant (Integer.MAX_VALUE - 5).
 * The annotation element must be that int, not an enum constant.
 */
public class AnnotationConstantFoldTest {
    @Retention(RetentionPolicy.RUNTIME)
    @interface Rank {
        int value();
    }

    @Rank(Precedence.ORDER)
    static class Target {
    }

    public static void main(String[] args) throws Exception {
        Rank rank = Target.class.getAnnotation(Rank.class);
        int want = Integer.MAX_VALUE - 5;
        if (rank == null || rank.value() != want) {
            throw new AssertionError(rank == null ? "missing" : String.valueOf(rank.value()));
        }
        System.out.println("PASS");
    }
}
