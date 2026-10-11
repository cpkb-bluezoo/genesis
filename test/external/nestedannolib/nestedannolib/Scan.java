package nestedannolib;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

@Retention(RetentionPolicy.RUNTIME)
public @interface Scan {
    Filter[] filters() default {};

    @Retention(RetentionPolicy.RUNTIME)
    @interface Filter {
        String value();
    }
}
