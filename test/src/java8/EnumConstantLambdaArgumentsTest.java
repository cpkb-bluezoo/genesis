import java.time.Duration;
import java.util.function.Function;

/* Regression test: lambdas and method references among the constructor arguments
 * of an enum constant are bound to the constructor's parameter types (Spring
 * Boot's DurationStyle: "MICROS(ChronoUnit.MICROS, "us", (duration) ->
 * duration.toNanos() / 1000L)" and "NANOS(..., Duration::toNanos)"). */
public class EnumConstantLambdaArgumentsTest {
    enum Style {
        NANOS("ns", Duration::toNanos),
        MICROS("us", (duration) -> duration.toNanos() / 1000L),
        MILLIS("ms", Duration::toMillis);

        final String suffix;
        final Function<Duration, Long> extractor;

        Style(String suffix, Function<Duration, Long> extractor) {
            this.suffix = suffix;
            this.extractor = extractor;
        }
    }

    public static void main(String[] args) {
        Duration d = Duration.ofNanos(5_000_000);
        if (Style.NANOS.extractor.apply(d) != 5_000_000L || Style.MICROS.extractor.apply(d) != 5_000L
                || Style.MILLIS.extractor.apply(d) != 5L || !"us".equals(Style.MICROS.suffix)) {
            throw new RuntimeException("enum lambdas");
        }
        System.out.println("EnumConstantLambdaArgumentsTest passed!");
    }
}
