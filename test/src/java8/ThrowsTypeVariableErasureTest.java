import java.lang.annotation.ElementType;
import java.lang.annotation.Target;
import java.lang.reflect.Method;
import java.util.function.Supplier;

/*
 * Regression test: "throws X" for "<T, X extends Throwable> @Marker T m(.., Supplier<? extends X>)"
 * (a type annotation before the return type) put a class called "X" in the Exceptions attribute
 * (NoClassDefFoundError: X when the class was transformed or reflected on): the class file names
 * X's erasure, its bound.
 */
public class ThrowsTypeVariableErasureTest {

    @Target(ElementType.TYPE_USE)
    @interface Marker {
    }

    static <T, X extends Throwable> @Marker T annotated(T value, Supplier<? extends X> supplier) throws X {
        if (value == null) {
            throw supplier.get();
        }
        return value;
    }

    static <T, X extends java.io.IOException> T plain(T value, Supplier<? extends X> supplier) throws X {
        return value;
    }

    public static void main(String[] args) throws Exception {
        Method annotated = ThrowsTypeVariableErasureTest.class.getDeclaredMethod("annotated", Object.class, Supplier.class);
        Method plain = ThrowsTypeVariableErasureTest.class.getDeclaredMethod("plain", Object.class, Supplier.class);
        if (annotated.getExceptionTypes().length != 1 || annotated.getExceptionTypes()[0] != Throwable.class ||
            plain.getExceptionTypes()[0] != java.io.IOException.class) {
            throw new RuntimeException("FAIL");
        }
        System.out.println("PASS");
    }
}
