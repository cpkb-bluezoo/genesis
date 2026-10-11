package p;

import java.nio.file.Path;
import java.util.List;

record Resource(Path path, boolean additional, List<String> tags) {
    Resource(Path path, boolean additional) {
        this(path, additional, List.of());
    }

    static Resource of(String name) {
        return new Resource(Path.of(name), false);
    }

    record Pair<A, B>(A first, B second) { }
}
