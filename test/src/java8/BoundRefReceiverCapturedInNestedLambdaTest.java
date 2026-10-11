import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

/* Regression test: the receiver of a bound method reference ("extractor::stackTrace")
 * is a local the enclosing lambdas must capture (Spring Boot's
 * ElasticCommonSchemaStructuredLogFormatter: "cannot resolve identifier: extractor"). */
public class BoundRefReceiverCapturedInNestedLambdaTest {
    static class Extractor {
        String stackTrace(String s) {
            return "<" + s + ">";
        }
    }

    static String run(Consumer<Consumer<String>> outer) {
        StringBuilder sb = new StringBuilder();
        outer.accept(sb::append);
        return sb.toString();
    }

    public static void main(String[] args) {
        Extractor extractor = new Extractor();
        String got = run((sink) -> {
            Supplier<Function<String, String>> s = () -> extractor::stackTrace;
            sink.accept(s.get().apply("z"));
            Runnable r = () -> sink.accept(((Function<String, String>) extractor::stackTrace).apply("y"));
            r.run();
        });
        if (!"<z><y>".equals(got)) {
            throw new RuntimeException(got);
        }
        System.out.println("BoundRefReceiverCapturedInNestedLambdaTest passed!");
    }
}
