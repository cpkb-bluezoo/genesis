import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/*
 * Regression test: "this::put" passed as a BiConsumer chose the overload put(String) (the first
 * one the class has under that name) instead of put(String, List): BootstrapMethodError when the
 * call site was linked. Same for "super::put".
 */
public class ThisMethodRefOverloadTest {

    static class Base {
        final Map<String, String> groups = new HashMap<>();

        void put(String group) {
            groups.put(group, group);
        }

        void put(String name, List<String> members) {
            put(name + members);
        }
    }

    static class Derived extends Base {
        void putAll(Map<String, List<String>> all) {
            all.forEach(this::put);
        }

        void putAllSuper(Map<String, List<String>> all) {
            all.forEach(super::put);
        }

        void put(int unrelated) {
        }
    }

    public static void main(String[] args) {
        Map<String, List<String>> all = new HashMap<>();
        all.put("x", Arrays.asList("y"));
        Derived derived = new Derived();
        derived.putAll(all);
        derived.putAllSuper(all);
        if (!"{x[y]=x[y]}".equals(derived.groups.toString())) {
            throw new RuntimeException("FAIL: " + derived.groups);
        }
        System.out.println("ThisMethodRefOverloadTest passed!");
    }
}
