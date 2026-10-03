/* Regression test: indexing the T[] returned by a generic method of a
 * wildcard-parameterized receiver - "Class<?> c; c.getEnumConstants()[0]".
 * The array's element type (T bound to the wildcard) was not recognized as a
 * reference type, so the element load used iaload on an Object[]
 * ("Bad type on operand stack in iaload"). Mirrors gumdrop's test
 * AnnotationStubs: "return rt.getEnumConstants()[0];". */
public class WildcardGenericArrayIndexVerifyTest {
    enum Color { RED, GREEN }

    static Object first(Class<?> rt) {
        if (rt.isEnum()) {
            return rt.getEnumConstants()[0];
        }
        return null;
    }

    static Object second(Class<? extends Enum<?>> rt) {
        return rt.getEnumConstants()[1];
    }

    static Object length(Class<?> rt) {
        Object[] all = rt.getEnumConstants();
        return all.length + rt.getEnumConstants().length;
    }

    public static void main(String[] args) {
        if (first(Color.class) != Color.RED) {
            throw new RuntimeException("first");
        }
        if (second(Color.class) != Color.GREEN) {
            throw new RuntimeException("second");
        }
        if (!Integer.valueOf(4).equals(length(Color.class))) {
            throw new RuntimeException("length");
        }
        if (first(String.class) != null) {
            throw new RuntimeException("non-enum");
        }
        System.out.println("WildcardGenericArrayIndexVerifyTest passed!");
    }
}
