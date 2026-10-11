/*
 * Regression test: "new T[n]" left the verifier's view of its stack slot as the int size (the
 * array replaced it in the bytecode but not in the frame): a conditional argument after it, whose
 * branch targets record a stack frame, then failed verification (Spring's
 * "new ApplicationStartedEvent(app, new String[0], context, (ms != null) ? .. : null)").
 */
public class NewArrayBeforeConditionalArgumentTest {

    static String seen = "";

    static void take(Object first, Object array, Object third, Object conditional) {
        seen += (array.getClass().getSimpleName()) + "/" + conditional;
    }

    static void primitive(boolean flag, int n) {
        take(null, new int[n], null, flag ? Integer.toString(n) : null);
    }

    static void reference(boolean flag, Long value) {
        take(null, new String[0], null, flag ? value.toString() : null);
    }

    static void nested(boolean flag, int n) {
        take(null, new String[n][], null, flag ? "x" : null);
        take(null, new int[2][n], null, flag ? "y" : null);
    }

    public static void main(String[] args) {
        primitive(true, 3);
        reference(true, 5L);
        reference(false, null);
        nested(true, 2);
        if (!"int[]/3String[]/5String[]/nullString[][]/xint[][]/y".equals(seen)) {
            throw new RuntimeException("FAIL: " + seen);
        }
        System.out.println("PASS");
    }
}
