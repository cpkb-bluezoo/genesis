/* Regression test: "int n = invoke(...)" with "<T> T invoke(...)": a primitive
 * target type makes T its wrapper, and the value is unboxed (Spring Boot's
 * ProcessInfo reads its virtual-thread counts this way). */
public class PrimitiveTargetGenericReturnTest {
    @SuppressWarnings("unchecked")
    static <T> T get(Object o) {
        return (T) o;
    }

    public static void main(String[] args) {
        int i = get(5);
        long l = get(6L);
        boolean b = get(true);
        double d = get(1.5);
        char c = get('c');
        if (i != 5 || l != 6L || !b || d != 1.5 || c != 'c') {
            throw new RuntimeException(i + " " + l + " " + b + " " + d + " " + c);
        }
        System.out.println("PrimitiveTargetGenericReturnTest passed!");
    }
}
