package nestedannotest;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/*
 * A RUNTIME-retained annotation type NESTED inside another class, compiled
 * to its own classfile first (see run-tests.sh) - mirrors the shape of
 * org.junit.runners.Parameterized.Parameters exactly, whose real classfile
 * path is "Parameterized$Parameters.class", not "Parameterized/Parameters.class".
 */
public class Markers {
    @Retention(RetentionPolicy.RUNTIME)
    @Target(ElementType.METHOD)
    public @interface Marker {
        String value() default "";
    }
}
