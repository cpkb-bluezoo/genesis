import java.lang.reflect.Method;
import lib.ExpectedThrows;

/*
 * Regression test: an annotation element whose value is a class literal
 * (`@ExpectedThrows(IllegalStateException.class)`, with the annotation
 * type loaded from its already-compiled .class file via -cp, not
 * -sourcepath - exactly as happens whenever a multi-module build forks
 * the compiler once per module) - exactly JUnit's own `@Test(expected =
 * SomeException.class)` shape, used throughout gumdrop's real test
 * suite - used to throw, the moment ANY code reflectively inspected
 * annotations on the annotated method (or, in gumdrop's case, JUnit's
 * own test-discovery machinery doing exactly that for every method in
 * the class):
 *
 *   java.lang.annotation.AnnotationFormatError: Unexpected end of
 *   annotations.
 *
 * and, once that was fixed, a second, different symptom:
 *
 *   java.lang.TypeNotPresentException: Type IllegalStateException not present
 *   Caused by: java.lang.ClassNotFoundException: IllegalStateException
 *
 * Root cause (two independent gaps, both needed together):
 *
 * 1. classwriter.c's write_annotation_value() (which encodes each
 *    element_value inside a RuntimeVisibleAnnotations attribute) had no
 *    case at all for a class-literal value (AST_CLASS_LITERAL) - unlike
 *    the analogous gap already fixed earlier for enum constants, this
 *    one was never covered. The "Unknown value type - skip" fallback
 *    silently wrote ZERO bytes for the value while the
 *    element_name_index and the annotation's own
 *    num_element_value_pairs count were already committed assuming a
 *    value WOULD follow - desyncing the rest of the annotation's binary
 *    layout so that every later reflective access to ANY annotation on
 *    the same construct failed outright. Fixed by adding a 'c'-tagged
 *    element_value (JVMS 4.7.16.1): a class_info_index pointing to a
 *    CONSTANT_Utf8 holding the literal's own type descriptor - not a
 *    CONSTANT_Class entry (unlike an ordinary ".class" literal used in
 *    code, via ldc). The matching pre-add constant-pool pass
 *    (preadd_annotation_cp_entries(), a separate function that must
 *    register the SAME constant pool entries before the pool itself is
 *    serialized) needed the identical fix, for the same reason already
 *    documented there for the long/float/double case.
 * 2. Even with (1) fixed, the class descriptor came out as the type
 *    node's bare, unqualified AST source name (e.g.
 *    "IllegalStateException" instead of the real
 *    "Ljava/lang/IllegalStateException;") - unlike an ordinary ".class"
 *    literal used in code, nothing ever visits an annotation's own
 *    class-literal value to resolve its type node via
 *    semantic_resolve_type() (annotation values are validated by
 *    dedicated, separate logic in semantic.c, not the general
 *    expression-visiting passes), so type_node->sem_type was reliably
 *    still NULL when ast_type_to_descriptor() looked for it. Fixed by
 *    forcing resolution (calling semantic_resolve_type() directly) when
 *    missing, in both the same two places as (1).
 */
public class AnnotationClassElementTest {
    @ExpectedThrows(IllegalStateException.class)
    public void mightThrow() {
    }

    public static void main(String[] args) throws Exception {
        Method m = AnnotationClassElementTest.class.getMethod("mightThrow");
        ExpectedThrows annotation = m.getAnnotation(ExpectedThrows.class);
        if (annotation == null) {
            throw new RuntimeException("expected annotation to be present");
        }
        Class<?> expected = annotation.value();
        if (expected != IllegalStateException.class) {
            throw new RuntimeException("expected IllegalStateException.class, got " + expected);
        }
        System.out.println("AnnotationClassElementTest passed!");
    }
}
