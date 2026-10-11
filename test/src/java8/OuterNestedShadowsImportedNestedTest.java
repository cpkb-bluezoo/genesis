import java.util.Iterator;
import java.util.Map;
import java.util.Properties;

/* Regression test: a class nested in a class that itself declares a member type
 * called Entry names THAT type, not the Map.Entry that "import java.util.Map"
 * would otherwise suggest ("interface Entry cannot be instantiated" in Spring
 * Boot's InfoProperties). */
public class OuterNestedShadowsImportedNestedTest implements Iterable<OuterNestedShadowsImportedNestedTest.Entry> {
    private final Properties properties;

    OuterNestedShadowsImportedNestedTest(Properties properties) {
        this.properties = properties;
    }

    @Override
    public Iterator<Entry> iterator() {
        return new PropertiesIterator(this.properties);
    }

    private static final class PropertiesIterator implements Iterator<Entry> {
        private final Iterator<Map.Entry<Object, Object>> iterator;

        private PropertiesIterator(Properties properties) {
            this.iterator = properties.entrySet().iterator();
        }

        @Override
        public boolean hasNext() {
            return this.iterator.hasNext();
        }

        @Override
        public Entry next() {
            Map.Entry<Object, Object> entry = this.iterator.next();
            return new Entry((String) entry.getKey(), (String) entry.getValue());
        }
    }

    public static final class Entry {
        final String key;
        final String value;

        private Entry(String key, String value) {
            this.key = key;
            this.value = value;
        }
    }

    public static void main(String[] args) {
        Properties p = new Properties();
        p.setProperty("k", "v");
        Entry e = new OuterNestedShadowsImportedNestedTest(p).iterator().next();
        if (!"k".equals(e.key) || !"v".equals(e.value)) {
            throw new RuntimeException(e.key + e.value);
        }
        System.out.println("OuterNestedShadowsImportedNestedTest passed!");
    }
}
