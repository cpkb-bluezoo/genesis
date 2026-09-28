package annotest;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/*
 * A minimal custom annotation with explicit RUNTIME retention, used by
 * AnnotationRetentionTest.java. This file is compiled to a .class first,
 * then AnnotationRetentionTest.java is compiled against it with -cp (not
 * -sourcepath), so its retention policy must be discovered the same way an
 * externally-defined annotation like JUnit's @Test is: by loading and
 * parsing ITS classfile's own @Retention meta-annotation, not by reading
 * source AST from the same compilation.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface RuntimeAnno {
}
