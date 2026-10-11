package r;

import p.Props;

/* The constant an annotation names is defined from another class (q.Limits) that this file does
 * not import: it is that file's imports that say what "Limits" is. */
@Marker(order = Props.ORDER, name = Props.NAME)
public class ImportedConstantInAnnotationTest {
    public static void main(String[] args) {
        Marker marker = ImportedConstantInAnnotationTest.class.getAnnotation(Marker.class);
        if (marker.order() != 995 || !"pre-name".equals(marker.name())) {
            throw new AssertionError(marker.order() + marker.name());
        }
        System.out.println("PASS");
    }
}
