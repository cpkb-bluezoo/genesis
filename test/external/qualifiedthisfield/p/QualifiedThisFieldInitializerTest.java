package p;

import java.util.Map;

/* "Outer.this.field" as a field initializer of an inner class, in a package: the class name in
 * it is the class around, not a type to qualify like those a constant is named by. */
public class QualifiedThisFieldInitializerTest {
    private final String value = "outer";

    private final Map<String, String> map = Map.of("k", "v");

    class Inner {
        private final String copy = QualifiedThisFieldInitializerTest.this.value;

        private final Map<String, String> viaMap = QualifiedThisFieldInitializerTest.this.map;

        private final Class<?> type = QualifiedThisFieldInitializerTest.class;

        String show() {
            return this.copy + this.viaMap.get("k") + this.type.getSimpleName().length();
        }
    }

    public static void main(String[] args) {
        String shown = new QualifiedThisFieldInitializerTest().new Inner().show();
        if (!"outerv".concat(String.valueOf("QualifiedThisFieldInitializerTest".length())).equals(shown)) {
            throw new AssertionError(shown);
        }
        System.out.println("PASS");
    }
}
