package annoelementslib;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

@Retention(RetentionPolicy.RUNTIME)
public @interface Ann {
    enum Mode { A, B, C }

    @Retention(RetentionPolicy.RUNTIME)
    @interface Sub {
        String value() default "sub-default";
    }

    String name() default "";
    String[] paths() default {};
    Mode mode() default Mode.A;
    Mode[] modes() default { Mode.B };
    long size() default -1L;
    int count() default 0;
    boolean flag() default false;
    double ratio() default 0.5;
    float weight() default 1.5f;
    char ch() default 'x';
    byte b() default 1;
    short sh() default 2;
    Class<?> type() default Object.class;
    Class<?>[] types() default {};
    Sub nested() default @Sub("d");
    Sub[] subs() default {};
}
