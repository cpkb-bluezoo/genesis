import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Collectors;

/* Regression test: method references whose target is a qualified or nested type
 * ("Map.Entry::getKey", "Outer.In::new", "java.util.Objects::isNull") were
 * resolved as a field access, and those naming an interface (Map.Entry::getKey,
 * List::size) used a Methodref where the method handle needs an
 * InterfaceMethodref, and a wildcard-typed receiver where it needs the declaring
 * type: Spring Boot has many of them. */
public class QualifiedMethodReferenceTargetTest {
    static class Outer {
        static class In {
            final String v;

            In() {
                v = "dflt";
            }

            In(String s) {
                v = s;
            }

            String get() {
                return v;
            }
        }
    }

    public static void main(String[] args) {
        Map<String, Integer> m = new TreeMap<>();
        m.put("a", 1);
        m.put("b", 2);
        check(m.entrySet().stream().map(Map.Entry::getKey).collect(Collectors.toList()).toString(), "[a, b]");
        check(m.entrySet().stream().map(java.util.Map.Entry::getValue).collect(Collectors.toList()).toString(), "[1, 2]");
        Supplier<Outer.In> s = Outer.In::new;
        Function<String, Outer.In> f = Outer.In::new;
        Function<Outer.In, String> g = Outer.In::get;
        check(s.get().v + f.apply("x").v + g.apply(f.apply("y")), "dfltxy");
        Predicate<Object> p = java.util.Objects::isNull;
        check(String.valueOf(p.test(null)), "true");
        Function<List<String>, Integer> size = List::size;
        check(String.valueOf(size.apply(List.of("a", "b"))), "2");
        System.out.println("QualifiedMethodReferenceTargetTest passed!");
    }

    static void check(String got, String want) {
        if (!want.equals(got)) {
            throw new RuntimeException("got " + got + ", want " + want);
        }
    }
}
