import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

/* Regression test: "Cust<String> c = new Plain<>() { }" where Plain<T>
 * implements Cust<T>. The target names a supertype of the diamond's class, so
 * the anonymous class's Signature must still name Plain<String> (frameworks
 * such as Spring read it back reflectively); it was left raw. */
public class DiamondAnonymousSupertypeTargetTest {
    interface Cust<T> {
        void customize(T t);
    }

    static class Plain<T> implements Cust<T> {
        public void customize(T t) {
        }
    }

    static Cust<String> viaReturn() {
        return new Plain<>() { };
    }

    private static void check(Object o, String expected) {
        Type t = o.getClass().getGenericSuperclass();
        String actual = t instanceof ParameterizedType
            ? ((ParameterizedType) t).getActualTypeArguments()[0].getTypeName() : "raw";
        if (!expected.equals(actual)) {
            System.out.println("FAILED: expected " + expected + ", got " + actual);
            System.exit(1);
        }
    }

    public static void main(String[] args) {
        Cust<Integer> local = new Plain<>() { };
        check(viaReturn(), "java.lang.String");
        check(local, "java.lang.Integer");
        System.out.println("DiamondAnonymousSupertypeTargetTest passed!");
    }
}
