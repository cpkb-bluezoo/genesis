import annotest.RuntimeAnno;
import java.lang.reflect.Method;

/*
 * Regression test: an annotation with RUNTIME retention that isn't one of
 * the JDK builtins classwriter.c's get_annotation_retention() hardcoded
 * (java.lang.Override, Deprecated, etc.) used to be silently dropped from
 * the classfile entirely. get_annotation_retention() only ever consulted a
 * fixed name table, defaulting to RETENTION_CLASS for anything else, and
 * classwriter.c's method-attribute writer only ever emits RETENTION_RUNTIME
 * annotations - so the annotation ended up written nowhere. This is why
 * JUnit's own @Test methods (RUNTIME retention, but unknown to genesis)
 * were invisible to reflection, and JUnit's test runner reported "No
 * runnable methods" for every gumdrop test class.
 *
 * Separately, the annotation's type name (e.g. "Test") was never resolved
 * against the compilation unit's imports before being written as the
 * classfile's type descriptor, so even a RUNTIME annotation that did get
 * written (like @Deprecated used bare) came out as the wrong/unresolvable
 * "LDeprecated;" instead of "Ljava/lang/Deprecated;". And an enum-valued
 * annotation element written as "Type.CONSTANT" (e.g. RetentionPolicy.
 * RUNTIME, needed by RuntimeAnno.java itself) was silently skipped in
 * write_annotation_value(), corrupting the annotation's binary layout.
 *
 * See genesis history for details (search "resolve_annotation_qualified_name"
 * and "semantic_resolve_annotation_retention" in classwriter.c/semantic.c).
 */
public class AnnotationRetentionTest {

    @RuntimeAnno
    public void annotated() {
    }

    public static void main(String[] args) throws Exception {
        Method m = AnnotationRetentionTest.class.getMethod("annotated");
        if (!m.isAnnotationPresent(RuntimeAnno.class)) {
            throw new RuntimeException(
                "@RuntimeAnno missing at runtime - retention policy not resolved correctly");
        }
        System.out.println("All tests passed!");
    }
}
