package p;

import java.io.IOException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

/* Regression test: a record declared in another file of the batch. Its
 * implicit accessors, canonical constructor and explicit members must be known
 * when this file is analysed ("cannot ... got <unknown>" for resource.path()),
 * whichever file is analysed first. Mirrors Spring Boot's test support
 * Resource record. */
public class RecordCrossFileTest {
    static URL urlOf(Resource resource) throws IOException {
        return resource.path().toUri().toURL();
    }

    public static void main(String[] args) throws IOException {
        Resource r = new Resource(Path.of("/tmp/x"), true, List.of("a", "b"));
        if (!r.additional() || r.tags().size() != 2 || !"x".equals(r.path().getFileName().toString())) {
            throw new RuntimeException("accessors");
        }
        Resource s = Resource.of("rel/name");
        if (s.additional() || !s.tags().isEmpty() || Files.isDirectory(s.path())) {
            throw new RuntimeException("convenience constructor");
        }
        if (!urlOf(r).toString().startsWith("file:")) {
            throw new RuntimeException("url");
        }
        Resource.Pair<String, Integer> pair = new Resource.Pair<>("k", 7);
        String k = pair.first();
        int v = pair.second();
        if (!"k".equals(k) || v != 7) {
            throw new RuntimeException("generic record");
        }
        Named n = new Named("abc", 3);
        if (!"ABC".equals(n.name()) || n.size() != 3) {
            throw new RuntimeException("explicit accessor");
        }
        if (!n.equals(new Named("abc", 3)) || n.hashCode() != new Named("abc", 3).hashCode()
                || !n.toString().contains("size=3")) {
            throw new RuntimeException("implicit members");
        }
        try {
            new Named("x", -1);
            throw new RuntimeException("compact constructor");
        } catch (IllegalArgumentException expected) {
            // expected
        }
        System.out.println("RecordCrossFileTest passed!");
    }
}
