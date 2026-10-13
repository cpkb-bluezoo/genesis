import java.lang.reflect.Modifier;

/* Regression test: a default constructor has the access of its class (JLS
 * 8.8.9). genesis made every one public, so a package-private class's
 * constructor was visible to reflection as public. */
public class DefaultConstructorAccessTest {
    public static class PublicClass {
    }

    protected static class ProtectedClass {
    }

    static class PackageClass {
    }

    private static class PrivateClass {
    }

    enum Letter {
        A
    }

    private static void check(Class<?> c, int expected) {
        int actual = c.getDeclaredConstructors()[0].getModifiers() & (Modifier.PUBLIC | Modifier.PROTECTED | Modifier.PRIVATE);
        if (actual != expected) {
            System.out.println("FAILED " + c.getName() + ": expected " + Modifier.toString(expected) + ", got " + Modifier.toString(actual));
            System.exit(1);
        }
    }

    public static void main(String[] args) {
        check(PublicClass.class, Modifier.PUBLIC);
        check(ProtectedClass.class, Modifier.PROTECTED);
        check(PackageClass.class, 0);
        check(PrivateClass.class, Modifier.PRIVATE);
        check(Letter.class, Modifier.PRIVATE);
        check(DefaultConstructorAccessTest.class, Modifier.PUBLIC);
        System.out.println("DefaultConstructorAccessTest passed!");
    }
}
