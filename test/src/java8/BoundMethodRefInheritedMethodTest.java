import java.util.Arrays;
import java.util.List;
import java.util.function.Consumer;

/* Regression test: a bound method reference to a method declared on a
 * super-interface of the variable's type (Spring Boot's
 * environment::containsProperty is PropertyResolver's, and
 * Arrays.asList(...)::forEach is Iterable's), against a "? super String"
 * parameter and an explicit "<String>" the metafactory is told about. */
public class BoundMethodRefInheritedMethodTest {
    interface Resolver {
        boolean has(String s);
    }

    interface Environment extends Resolver {
    }

    static class Env implements Environment {
        public boolean has(String s) {
            return "a".equals(s);
        }
    }

    static <T> void each(Consumer<Consumer<T>> source, Consumer<T> sink) {
        source.accept(sink);
    }

    public static void main(String[] args) {
        Environment env = new Env();
        List<String> l = List.of("a", "b");
        StringBuilder sb = new StringBuilder();
        BoundMethodRefInheritedMethodTest.<String>each(Arrays.asList("x", "y")::forEach, sb::append);
        if (l.stream().allMatch(env::has) || !l.stream().anyMatch(env::has) || !"xy".equals(sb.toString())) {
            throw new RuntimeException("bound refs " + sb);
        }
        System.out.println("BoundMethodRefInheritedMethodTest passed!");
    }
}
