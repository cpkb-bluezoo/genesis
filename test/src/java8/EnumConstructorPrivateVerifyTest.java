import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;

/* Regression test: an enum's constructor is implicitly private (JLS
 * 8.9.2) whether or not the source says so, but genesis wrote it to the
 * class file with whatever access the source spelled out - so a
 * constructor declared with no modifier came out package-private. javac
 * always marks it ACC_PRIVATE.
 *
 * Deliberately no enum constant with a class body and no varargs enum
 * constructor here: both are separate genesis gaps. */
public class EnumConstructorPrivateVerifyTest {

    enum Implicit {
        A("a"), B;

        final String label;

        Implicit(String label) {
            this.label = label;
        }

        Implicit() {
            this("");
        }
    }

    enum Explicit {
        X(1);

        final int n;

        private Explicit(int n) {
            this.n = n;
        }
    }

    enum NoConstructor {
        ONLY
    }

    private static void check(Class<?> type) {
        Constructor<?>[] ctors = type.getDeclaredConstructors();
        if (ctors.length == 0) {
            throw new RuntimeException(type.getName() + ": no constructors");
        }
        for (Constructor<?> c : ctors) {
            if (!Modifier.isPrivate(c.getModifiers())) {
                throw new RuntimeException(type.getName() + ": constructor is not private: " + c);
            }
        }
    }

    public static void main(String[] args) {
        check(Implicit.class);
        check(Explicit.class);
        check(NoConstructor.class);
        if (!"a".equals(Implicit.A.label) || !"".equals(Implicit.B.label)
                || Explicit.X.n != 1) {
            throw new RuntimeException("enum constants built wrongly");
        }
        System.out.println("PASS");
    }
}
