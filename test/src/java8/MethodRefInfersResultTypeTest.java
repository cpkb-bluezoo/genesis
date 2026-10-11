import java.time.Instant;
import java.time.format.DateTimeFormatter;
import java.time.temporal.TemporalQuery;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;

/* Regression test: a method reference passed to a generic method says what the
 * method's type variable is (what the referenced method returns), and is never
 * a candidate for a class-typed parameter of an overload:
 * DateTimeFormatter.ISO_INSTANT.parse(text, Instant::from) is the
 * parse(CharSequence, TemporalQuery<T>) overload, not
 * parse(CharSequence, ParsePosition); an array constructor reference does the
 * same (Stream.toArray(String[]::new) is a String[]), and a bound one is typed by
 * its receiver (String.class::cast is a String). */
public class MethodRefInfersResultTypeTest {
    static <T> T query(String s, TemporalQuery<T> q) {
        return q.queryFrom(DateTimeFormatter.ISO_INSTANT.parse(s));
    }

    static <T> T apply(String s, Function<String, T> f) {
        return f.apply(s);
    }

    public static void main(String[] args) {
        long millis = DateTimeFormatter.ISO_INSTANT.parse("1970-01-01T00:00:01Z", Instant::from).toEpochMilli();
        long viaQuery = query("1970-01-01T00:00:02Z", Instant::from).toEpochMilli();
        int length = apply("four", String::length).intValue();
        int parsed = apply("42", Integer::valueOf).intValue();
        StringBuilder sb = apply("x", StringBuilder::new).append("y");
        String[] words = List.of("a", "b").stream().map(String::toUpperCase).toArray(String[]::new);
        if (words.length != 2 || !"B".equals(words[1])) {
            throw new RuntimeException("toArray(String[]::new)");
        }
        int castLength = Optional.<Object>of("four").map(String.class::cast).map(String::length).orElse(0);
        if (castLength != 4) {
            throw new RuntimeException("Class::cast");
        }
        if (millis != 1000 || viaQuery != 2000 || length != 4 || parsed != 42 || !"xy".contentEquals(sb)) {
            throw new RuntimeException(millis + " " + viaQuery + " " + length + " " + parsed + " " + sb);
        }
        System.out.println("MethodRefInfersResultTypeTest passed!");
    }
}
