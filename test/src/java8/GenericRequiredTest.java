/** Static &lt;T&gt; T required(T) return type inference. */
public class GenericRequiredTest {
    static <T> T required(T value) {
        if (value == null) {
            throw new IllegalArgumentException("null");
        }
        return value;
    }

    static long fromLong(Long n) {
        return required(n).longValue();
    }

    static long fromLongLocal(Long n) {
        long v = required(n).longValue();
        return v;
    }

    public static void main(String[] args) {
        System.out.println("GenericRequiredTest OK " + fromLong(3L) + " " + fromLongLocal(4L));
    }
}
