import emptyutils.Utils;
import java.util.function.Predicate;

/*
 * Regression test: "Predicate<T> isEmpty = Utils::isEmpty" in a generic class, for the overloads
 * isEmpty(Object[]) and isEmpty(Object) of a class file, took the array one (a type variable of
 * the code in hand fitted any parameter): BootstrapMethodError, "Object is not convertible to
 * Object[]". T is its bound, Object, which is no array.
 */
public class TypeVariableMethodReferenceOverloadTest {

    static class Member<T> {
        boolean emptyValue(T value) {
            Predicate<T> isEmpty = Utils::isEmpty;
            return isEmpty.test(value);
        }
    }

    static <V extends CharSequence> boolean emptyBounded(V value) {
        Predicate<V> isEmpty = Utils::isEmpty;
        return isEmpty.test(value);
    }

    public static void main(String[] args) {
        boolean actual = new Member<String>().emptyValue("") && !new Member<String>().emptyValue("x") && emptyBounded("");
        if (!actual) {
            throw new RuntimeException("FAIL");
        }
        System.out.println("PASS");
    }
}
