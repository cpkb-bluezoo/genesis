import java.io.InputStream;
import java.util.ArrayList;
import java.util.Map;

/*
 * "var files = new ArrayList<>(map.values())" is an ArrayList of the map's
 * value type (the constructor takes a Collection<? extends E>), so passing an
 * element on picks the overload for that type, not the InputStream one.
 */
public class VarDiamondCollectionArgOverloadTest {
    static class Doc {
        final String name;

        Doc(String name) {
            this.name = name;
        }
    }

    static String store(Doc doc) {
        return "doc:" + doc.name;
    }

    static String store(InputStream in) {
        return "stream";
    }

    public static void main(String[] args) {
        Map<String, Doc> extra = Map.of("k", new Doc("a"));
        var files = new ArrayList<>(extra.values());
        files.add(new Doc("b"));
        StringBuilder out = new StringBuilder();
        for (final var file : files) {
            var result = store(file);
            out.append(result).append(';');
        }
        String got = out.toString();
        if (!got.equals("doc:a;doc:b;")) {
            throw new AssertionError(got);
        }
        System.out.println("VarDiamondCollectionArgOverloadTest passed!");
    }
}
