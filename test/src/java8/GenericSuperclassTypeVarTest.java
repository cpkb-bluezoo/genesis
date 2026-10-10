import java.util.ArrayList;

/* Regression test: a class's own type parameters are in scope in its extends
 * and implements clauses ("class Sub<T> extends Box<T>", and the self-bounded
 * "Mid<E extends Mid<E>> extends Base<E>" shape). They used to be resolved in
 * the file scope and failed with "Cannot resolve type: T". Mirrors gumdrop's
 * OtlpCollectorExporter extends BatchingExporter. */
public class GenericSuperclassTypeVarTest {
    static abstract class Base<E extends Base<E>> {
        String name = "base";
        @SuppressWarnings("unchecked")
        E self() { return (E) this; }
        E named(String n) { name = n; return self(); }
    }

    static abstract class Mid<E extends Mid<E>> extends Base<E> {
        int level;
        E level(int l) { level = l; return self(); }
    }

    static class Leaf extends Mid<Leaf> {
        String show() { return name + ":" + level; }
    }

    static class Box<X> {
        X value;
        X get() { return value; }
    }

    static class Sub<T> extends Box<T> implements Comparable<Sub<T>> {
        public int compareTo(Sub<T> o) { return 0; }
    }

    static class Names<T> extends ArrayList<T> { }

    class Inner extends Box<String> { }

    public static void main(String[] args) {
        Leaf l = new Leaf().named("x").level(3);
        if (!"x:3".equals(l.show())) {
            throw new RuntimeException(l.show());
        }
        Sub<String> s = new Sub<String>();
        s.value = "v";
        String got = s.get();
        if (!"v".equals(got)) {
            throw new RuntimeException(got);
        }
        Names<Integer> n = new Names<Integer>();
        n.add(4);
        int first = n.get(0);
        if (first != 4) {
            throw new RuntimeException("names " + first);
        }
        System.out.println("GenericSuperclassTypeVarTest passed!");
    }
}
