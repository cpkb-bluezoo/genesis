/*
 * Regression test: "flag ? 1 : null" was typed int, so the boxing happened after the two
 * branches met: Integer.valueOf(null) (NullPointerException, or a VerifyError when the frame
 * at the join was checked). The conditional is an Integer: the primitive branch is boxed.
 */
public class ConditionalPrimitiveOrNullTest {

    static Integer fromCast(Long value) {
        return (value != null) ? (int) value.longValue() : null;
    }

    static Long fromCall(Long value) {
        return (value != null) ? value.longValue() : null;
    }

    static Object asObject(Long value) {
        return (value != null) ? value.intValue() : null;
    }

    static String local(boolean flag) {
        Integer value = flag ? 1 : null;
        return "" + value;
    }

    public static void main(String[] args) {
        String actual = fromCast(5L) + " " + fromCast(null) + " " + fromCall(6L) + " " + fromCall(null)
            + " " + asObject(7L) + " " + asObject(null) + " " + local(true) + local(false);
        if (!"5 null 6 null 7 null 1null".equals(actual)) {
            throw new RuntimeException("FAIL: " + actual);
        }
        System.out.println("ConditionalPrimitiveOrNullTest passed!");
    }
}
