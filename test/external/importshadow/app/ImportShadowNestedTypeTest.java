package importshadow.app;

import importshadow.other.Encoding;

/* Regression test: a single-type import names the type "Encoding" for the
 * whole file, even when a class of the same package (Props) declares a nested
 * type of that name and is itself part of the compilation. The class literal in
 * an annotation resolved to Props.Encoding, so Spring's @ConditionalOnClass
 * tested the wrong class. */
@Marker(Encoding.class)
public class ImportShadowNestedTypeTest {
    Props props;

    public static void main(String[] args) {
        Class<?> named = ImportShadowNestedTypeTest.class.getAnnotation(Marker.class).value()[0];
        if (named != Encoding.class) {
            System.out.println("FAILED: annotation names " + named.getName());
            System.exit(1);
        }
        System.out.println("ImportShadowNestedTypeTest passed!");
    }
}
