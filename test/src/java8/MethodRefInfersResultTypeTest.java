import java.time.Instant;
import java.time.format.DateTimeFormatter;
import java.time.temporal.TemporalQuery;
import java.util.function.Function;

/* Regression test: a method reference passed to a generic method says what the
 * method's type variable is (what the referenced method returns), and is never
 * a candidate for a class-typed parameter of an overload:
 * DateTimeFormatter.ISO_INSTANT.parse(text, Instant::from) is the
 * parse(CharSequence, TemporalQuery<T>) overload, not
 * parse(CharSequence, ParsePosition). */
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
        if (millis != 1000 || viaQuery != 2000 || length != 4 || parsed != 42 || !"xy".contentEquals(sb)) {
            throw new RuntimeException(millis + " " + viaQuery + " " + length + " " + parsed + " " + sb);
        }
        System.out.println("MethodRefInfersResultTypeTest passed!");
    }
}
