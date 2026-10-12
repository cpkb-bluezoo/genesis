/**
 * Regression: Type.class must resolve to a Class parameter, not Object
 * (same scoring issue as ReflectionTestUtils.invokeMethod(Class, ...)).
 */
public class ClassLiteralOverloadVerifyTest {
    static String which(Object ignored) {
        return "object";
    }

    static String which(Class<?> ignored) {
        return "class";
    }

    public static void main(String[] args) {
        if (!"class".equals(which(String.class))) {
            throw new RuntimeException("class literal picked wrong overload");
        }
        System.out.println("ClassLiteralOverloadVerifyTest passed!");
    }
}
