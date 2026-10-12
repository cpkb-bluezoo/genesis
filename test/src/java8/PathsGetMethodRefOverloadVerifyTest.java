import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * Regression: Paths::get for Adapter&lt;String&gt; must name get(String), not get(URI).
 * Wrong choice caused LambdaConversionException in ReactiveMultipartAutoConfiguration.
 */
public class PathsGetMethodRefOverloadVerifyTest {
    @FunctionalInterface
    interface Adapter<T> {
        Path adapt(T value);
    }

    public static void main(String[] args) {
        Adapter<String> a = Paths::get;
        Path p = a.adapt("/tmp");
        if (p == null || !p.toString().endsWith("tmp")) {
            throw new RuntimeException("bad path: " + p);
        }
        System.out.println("PathsGetMethodRefOverloadVerifyTest passed!");
    }
}
