import java.util.function.Supplier;

/*
 * Regression test: "members.from(Person::lastName)" with overloads from(V), from(Supplier<V>) and
 * from(Extractor<T, V>) took the Supplier one (BootstrapMethodError: LambdaConversionException,
 * an instance method needs a receiver): "Type::instance" fits only an interface whose method takes
 * the receiver as its first parameter (JLS 15.12.2.1).
 */
public class MethodReferenceArityOverloadTest {

    interface Extractor<T, V> {
        V extract(T value);
    }

    static class Members<T> {
        <V> String from(V value) {
            return "value";
        }

        <V> String from(Supplier<V> supplier) {
            return "supplier";
        }

        <V> String from(Extractor<T, V> extractor) {
            return "extractor";
        }
    }

    static class Person {
        String lastName() {
            return "Boot";
        }

        static String shout() {
            return "HEY";
        }

        String bound(String suffix) {
            return suffix;
        }
    }

    public static void main(String[] args) {
        Members<Person> members = new Members<Person>();
        Person person = new Person();
        String actual = members.from(Person::lastName) + " " + members.from(Person::shout) + " " + members.from(person::lastName);
        if (!"extractor supplier supplier".equals(actual)) {
            throw new RuntimeException("FAIL: " + actual);
        }
        System.out.println("PASS");
    }
}
