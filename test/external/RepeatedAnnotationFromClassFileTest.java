import repeatablelib.Tag;

/* An annotation read from a class file whose own @Repeatable names its container, written twice
 * here: the class file has one container annotation holding both. */
@Tag("first")
@Tag("second")
public class RepeatedAnnotationFromClassFileTest {
    @Tag("a")
    @Tag("b")
    void method() {
    }

    public static void main(String[] args) throws Exception {
        Tag[] onClass = RepeatedAnnotationFromClassFileTest.class.getAnnotationsByType(Tag.class);
        Tag[] onMethod = RepeatedAnnotationFromClassFileTest.class.getDeclaredMethod("method")
            .getAnnotationsByType(Tag.class);
        if (onClass.length != 2 || !"second".equals(onClass[1].value()) || onMethod.length != 2
                || !"a".equals(onMethod[0].value())) {
            throw new AssertionError(onClass.length + " " + onMethod.length);
        }
        System.out.println("PASS");
    }
}
