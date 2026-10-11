import nestedannolib.Scan;

/* A nested annotation type of an imported class, written "Scan.Filter" in the source: its type
 * descriptor in the class file has to be the real binary name (nestedannolib/Scan$Filter). */
@Scan(filters = @Scan.Filter("one"))
public class NestedAnnotationNameFromClassFileTest {
    public static void main(String[] args) {
        Scan scan = NestedAnnotationNameFromClassFileTest.class.getAnnotation(Scan.class);
        if (scan == null || scan.filters().length != 1 || !"one".equals(scan.filters()[0].value())) {
            throw new AssertionError("annotation not readable");
        }
        System.out.println("PASS");
    }
}
