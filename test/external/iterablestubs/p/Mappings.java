package p;

import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;

public class Mappings implements Iterable<Mappings.Mapping> {
    private final Map<String, Mapping> mappings = new LinkedHashMap<>();

    public String add(String extension, String type) {
        mappings.put(extension, new Mapping(extension, type));
        return type;
    }

    public final Iterator<Mapping> iterator() {
        return mappings.values().iterator();
    }

    public static final class Mapping {
        private final String extension;
        private final String type;

        public Mapping(String extension, String type) {
            this.extension = extension;
            this.type = type;
        }

        public String getExtension() {
            return extension;
        }

        public String getType() {
            return type;
        }
    }
}
