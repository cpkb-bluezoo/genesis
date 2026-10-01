import classlitlib.Marker;

import java.lang.reflect.Method;

/*
 * Bug: a class literal for a NESTED type used as an annotation value (e.g.
 * "@Test(expected = Outer.Nested.class)") wrote the wrong classfile
 * descriptor: "LOuter/Nested;" instead of "LOuter$Nested;". Root cause:
 * "Outer.Nested.class" parses its type portion as an AST_FIELD_ACCESS chain
 * (AST_IDENTIFIER "Outer" -> AST_FIELD_ACCESS "Nested"), not AST_CLASS_TYPE -
 * ast_type_to_descriptor()'s AST_FIELD_ACCESS case never checked the node's
 * own (already correctly resolved, by the time this runs) sem_type at all,
 * unlike its AST_CLASS_TYPE/AST_IDENTIFIER sibling cases - it unconditionally
 * rebuilt the descriptor from the raw chain instead, joining every segment
 * with '/'. This dropped the type's real package entirely (the source chain
 * never includes one) and used '/' where a nested class needs '$' - genesis's
 * own classfile-writing completed with no complaint, but reflection on any
 * annotation using this shape throws java.lang.TypeNotPresentException, since
 * the descriptor names a class that doesn't exist. Confirmed against
 * gumdrop's own MessageIndexTest, whose
 * "@Test(expected = MessageIndex.CorruptIndexException.class)" depends on
 * exactly this.
 */
public class NestedClassLiteralAnnotationTest {
    static class Nested extends RuntimeException {
    }

    @Marker(NestedClassLiteralAnnotationTest.Nested.class)
    void annotated() {
    }

    public static void main(String[] args) throws Exception {
        Method m = NestedClassLiteralAnnotationTest.class.getDeclaredMethod("annotated");
        Marker marker = m.getAnnotation(Marker.class);
        if (marker == null) {
            throw new RuntimeException("expected @Marker to be present via reflection, got null");
        }
        if (marker.value() != Nested.class) {
            throw new RuntimeException("expected value()==Nested.class, got " + marker.value());
        }
        System.out.println("NestedClassLiteralAnnotationTest passed!");
    }
}
