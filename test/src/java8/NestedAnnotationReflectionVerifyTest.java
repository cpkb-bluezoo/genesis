import java.lang.annotation.*;
import java.lang.reflect.Method;

/*
 * GitHub issue #1: a method annotated with an annotation type declared as
 * a NESTED type (an @interface nested inside a regular class) compiled
 * cleanly, but Method.getAnnotation(...) always returned null at runtime
 * instead of the real annotation instance - no RuntimeVisibleAnnotations
 * attribute was written for it at all.
 *
 * Root cause: semantic_resolve_annotation_retention() (semantic.c), used
 * by classwriter.c's get_annotation_retention() to find a non-JDK-builtin
 * annotation's own @Retention meta-annotation, only ever checked the
 * CLASSPATH (an already-compiled .class file) - there is no classfile yet
 * for an annotation type being compiled in the SAME batch as the code
 * using it, which is unavoidable for a NESTED annotation type (it can
 * only ever be declared inside its enclosing class's own file, compiled
 * together with it). resolve_import() also had no path at all for "this
 * bare name is a nested type of the class currently being compiled" (as
 * opposed to a nested type reached through an explicit import) - so
 * "qualified" came back NULL and the classpath lookup was never even
 * attempted. Either way, retention silently defaulted to CLASS (not
 * runtime-visible), so the annotation was dropped from the classfile
 * entirely.
 *
 * Fixed by: (1) re-pointing sem->current_class at the class actually
 * being written during the codegen/write phase (classwriter.c's
 * write_class_bytes()), since semantic analysis has already finished for
 * every file by then and current_class is left wherever pass2 last used
 * it; (2) adding a same-batch fallback to
 * semantic_resolve_annotation_retention() that searches that class's own
 * nested-type members (and its enclosing/superclass chain) for the
 * annotation type by simple name, and reads its real @Retention
 * meta-annotation directly from its own source AST (rather than a
 * classfile) when found that way.
 */
public class NestedAnnotationReflectionVerifyTest {
    @Retention(RetentionPolicy.RUNTIME)
    @Target(ElementType.METHOD)
    @interface Marker {
        int value();
    }

    @Marker(42)
    public void foo() {}

    public static void main(String[] args) throws Exception {
        Method m = NestedAnnotationReflectionVerifyTest.class.getMethod("foo");
        Marker marker = m.getAnnotation(Marker.class);
        if (marker == null) {
            throw new RuntimeException("@Marker missing at runtime - nested annotation retention not resolved");
        }
        if (marker.value() != 42) {
            throw new RuntimeException("expected 42 but got " + marker.value());
        }
        System.out.println("NestedAnnotationReflectionVerifyTest passed!");
    }
}
