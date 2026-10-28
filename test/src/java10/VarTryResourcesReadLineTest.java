import java.io.BufferedReader;
import java.io.StringReader;
import java.util.ArrayList;

/**
 * Minimal pattern: {@code var} in try-with-resources, {@code var} from {@code readLine()},
 * then {@code String.contains} in an {@code if} condition inside a {@code while} loop.
 */
public class VarTryResourcesReadLineTest {
    public static void main(String[] args) throws Exception {
        final var signature = "MARKER";
        final var hits = new ArrayList<String>();

        try (final var reader = new BufferedReader(new StringReader("before\nMARKER-line\n"))) {
            while (true) {
                final var line = reader.readLine();
                if (line == null) {
                    break;
                }
                if (line.contains(signature)) {
                    hits.add("found");
                }
            }
        }

        if (hits.size() != 1) {
            throw new AssertionError();
        }
        System.out.println("VarTryResourcesReadLineTest passed!");
    }
}
