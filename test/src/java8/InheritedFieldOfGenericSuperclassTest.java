import java.util.HashMap;
import java.util.Map;

/* Regression test: a field inherited from a generic superclass has the type the
 * superclass's argument makes it - "T source" of "Base<T>" is a Map in
 * "Derived extends Base<Map<String, Object>>", whether named as source or
 * this.source (Spring Boot's MapWithNullsPropertySource.source). */
abstract class FieldBase<T> {
    protected final T source;

    FieldBase(T source) {
        this.source = source;
    }
}

abstract class FieldMid<K, V> extends FieldBase<Map<K, V>> {
    FieldMid(Map<K, V> m) {
        super(m);
    }
}

public class InheritedFieldOfGenericSuperclassTest extends FieldMid<String, Object> {
    InheritedFieldOfGenericSuperclassTest(Map<String, Object> m) {
        super(m);
    }

    Object viaName(String k) {
        return source.get(k);
    }

    Object viaThis(String k) {
        return this.source.get(k);
    }

    boolean has(String k) {
        return this.source.containsKey(k);
    }

    String[] keys() {
        return source.keySet().toArray(new String[0]);
    }

    public static void main(String[] args) {
        Map<String, Object> m = new HashMap<>();
        m.put("a", 1);
        InheritedFieldOfGenericSuperclassTest t = new InheritedFieldOfGenericSuperclassTest(m);
        if (!Integer.valueOf(1).equals(t.viaName("a")) || !Integer.valueOf(1).equals(t.viaThis("a"))
                || !t.has("a") || t.has("b") || t.keys().length != 1) {
            throw new RuntimeException("inherited field");
        }
        System.out.println("InheritedFieldOfGenericSuperclassTest passed!");
    }
}
