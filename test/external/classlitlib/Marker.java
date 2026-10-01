package classlitlib;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/*
 * Compiled to its own classfile first (see run-tests.sh) so its RUNTIME
 * retention is discovered from its own classfile, exactly like
 * org.junit.Test - the actual bug this supports testing is unrelated to
 * retention (it's about the class-literal VALUE's own descriptor), but a
 * same-compilation-unit annotation type's retention isn't reliably
 * resolved (a separate, pre-existing gap, not fixed here), so this needs
 * the same two-step compile as annotest/nestedannotest to even reach the
 * bug being tested.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface Marker {
    Class<?> value();
}
