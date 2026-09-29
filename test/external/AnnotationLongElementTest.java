/*
 * Regression test: an annotation whose element is declared with a wider
 * numeric type than an int literal (e.g. "long timeout()", used as
 * "@Timed(timeout = 10000)" - exactly JUnit's own "@Test(timeout =
 * 10000)"), where the annotation type is loaded from its already-
 * compiled .class file (via -cp, not -sourcepath - exactly as happens
 * whenever a multi-module build forks the compiler once per module).
 * JLS allows an int constant expression to initialize a long-typed
 * annotation element (implicit widening), but this used to produce a
 * classfile whose RuntimeVisibleAnnotations attribute stored the value
 * with the wrong element_value tag - discovered via a real reflective
 * read throwing:
 *
 *   java.lang.annotation.AnnotationTypeMismatchException: Incorrectly
 *   typed data found for annotation element ... timeout()
 *   (Found data of type java.lang.Integer[10000])
 *
 * Root cause: write_annotation_value() in classwriter.c always wrote a
 * TOK_INTEGER_LITERAL annotation value with tag 'I' (a CONSTANT_Integer
 * entry), regardless of the annotation element's own declared return
 * type - so a long/float/double-returning element with an int-literal
 * value got a mismatched tag the JVM's reflection code rejects at
 * annotation-parse time (the tag must match the element method's real
 * return type, not just the literal's own natural int shape). Fixed by
 * resolving the element's declared return type (via a new
 * classfile_get_method_descriptor() + semantic_resolve_annotation_
 * element_descriptor(), reading the annotation interface's own
 * classfile) and emitting the correctly-tagged/widened constant ('J'/
 * 'F'/'D' via cp_add_long()/cp_add_float()/cp_add_double() as needed).
 *
 * Fixing that alone then surfaced a second, latent bug: the classfile
 * writer pre-adds constant pool entries for every annotation value in a
 * separate pass (preadd_annotation_cp_entries()) *before* the constant
 * pool itself is serialized to the output buffer - that pre-add pass
 * still assumed every int literal was a plain CONSTANT_Integer, so the
 * *first* time a long constant was actually needed was during the real,
 * later write pass, by which point the constant pool bytes were already
 * flushed - producing a classfile whose annotation attribute referenced
 * a constant pool index that was never written at all:
 *
 *   java.lang.IllegalArgumentException: Constant pool index out of
 *   bounds (thrown from AnnotationParser.parseConst)
 *
 * Fixed by (1) mirroring the same declared-return-type resolution in
 * preadd_annotation_cp_entries() so the correctly-typed entry is added
 * up front, and (2) adding value-based deduplication to cp_add_long()/
 * cp_add_float()/cp_add_double() (mirroring cp_add_integer()'s existing
 * dedup) so the real write pass's later call finds and reuses that same
 * pre-added entry instead of allocating a second, too-late one.
 */
import lib.Timed;

public class AnnotationLongElementTest {
    @Timed(timeout = 10000)
    public void run() {}

    public static void main(String[] args) throws Exception {
        Timed t = AnnotationLongElementTest.class.getMethod("run").getAnnotation(Timed.class);
        long v = t.timeout();
        if (v != 10000L) {
            throw new RuntimeException("expected 10000, got " + v);
        }
        System.out.println("AnnotationLongElementTest passed!");
    }
}
