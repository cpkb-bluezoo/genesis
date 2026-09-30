import java.lang.annotation.Annotation;
import java.lang.reflect.Method;

/*
 * A RUNTIME-retained annotation type NESTED inside another class (e.g.
 * "Markers.Marker", or real-world org.junit.runners.Parameterized.Parameters)
 * was silently dropped from any method it annotated - no RuntimeVisibleAnnotations
 * entry at all, visible or invisible, with no diagnostic. Root cause:
 * semantic_resolve_annotation_retention() and semantic_resolve_annotation_
 * element_descriptor() (semantic.c) resolved the annotation type's qualified
 * name (e.g. "nestedannotest.Markers.Marker") and passed it straight to
 * classpath_load_class(), which naively converts every '.' to '/' - but a
 * nested type's real classfile path uses '$' at the nesting boundary
 * ("Markers$Marker.class"), not '/' ("Markers/Marker.class"). The classfile
 * was never found, so its real @Retention meta-annotation was never read -
 * unlike load_external_class_impl(), which already has a dot-to-$ candidate
 * fallback for exactly this, that these two functions never went through.
 *
 * Fixing just the retention lookup surfaced a second, related bug:
 * write_annotation() and preadd_annotation_cp_entries() (classwriter.c) had
 * their OWN separate copies of the same naive dot-to-slash conversion for
 * the annotation's TYPE DESCRIPTOR (not just its retention). Even once the
 * annotation was correctly included, its descriptor named a nonexistent
 * class ("Lnestedannotest/Markers/Marker;") - which the JVM's own
 * annotation parser tolerates by silently excluding the annotation from
 * getAnnotations()/getAnnotation() rather than erroring, so this was
 * invisible at both compile time and class-load time too. Both call sites
 * now prefer the actual classfile's own authoritative binary name
 * (classfile_t::this_class_name) when the type can be loaded from the
 * classpath, falling back to the naive conversion only when it can't (e.g.
 * a still source-defined type).
 *
 * Confirmed against gumdrop's own DecoderTest/EncoderTest, whose data()
 * method's "@org.junit.runners.Parameterized.Parameters(name = "...")" was
 * dropped entirely, so JUnit's Parameterized runner found no @Parameters
 * method and failed every test in both suites with "No public static
 * parameters method".
 */
public class NestedAnnotationRetentionTest {

    @nestedannotest.Markers.Marker(value = "hello")
    public void annotated() {
    }

    public static void main(String[] args) throws Exception {
        Method m = NestedAnnotationRetentionTest.class.getMethod("annotated");
        Annotation[] annotations = m.getAnnotations();
        Annotation found = null;
        for (Annotation a : annotations) {
            if (a.annotationType().getName().equals("nestedannotest.Markers$Marker")) {
                found = a;
                break;
            }
        }
        if (found == null) {
            throw new RuntimeException(
                "no nestedannotest.Markers$Marker annotation found via reflection; got " +
                annotations.length + " annotations");
        }
        Object value = found.annotationType().getMethod("value").invoke(found);
        if (!"hello".equals(value)) {
            throw new RuntimeException("expected value()==\"hello\", got " + value);
        }
        System.out.println("All tests passed!");
    }
}
