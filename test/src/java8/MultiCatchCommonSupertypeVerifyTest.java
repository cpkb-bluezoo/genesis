import java.lang.reflect.InvocationTargetException;

/* Regression test: the parameter of a multi-catch clause has the least
 * upper bound of the alternatives' types (JLS 14.20), not Object. Five
 * reflective exceptions all share ReflectiveOperationException; genesis
 * typed the catch variable as Object, so passing it to a Throwable
 * parameter failed verification ("Type 'java/lang/Object' is not
 * assignable to 'java/lang/Throwable'"). Mirrors gumdrop's Context.init(),
 * "JulWarnings.severe(LOGGER, message, e)". */
public class MultiCatchCommonSupertypeVerifyTest {
    static String last;

    static void record(String message, Throwable t) {
        last = message + ":" + t.getClass().getSimpleName();
    }

    static void fail(int which) throws Exception {
        switch (which) {
            case 0: throw new ClassNotFoundException("a");
            case 1: throw new InstantiationException("b");
            case 2: throw new IllegalAccessException("c");
            case 3: throw new InvocationTargetException(null);
            default: throw new NoSuchMethodException("e");
        }
    }

    public static void main(String[] args) throws Exception {
        for (int i = 0; i < 5; i++) {
            try {
                fail(i);
            } catch (ClassNotFoundException | InstantiationException | IllegalAccessException
                    | InvocationTargetException | NoSuchMethodException e) {
                record("m", e);
                ReflectiveOperationException roe = e;
                if (roe == null) {
                    throw new RuntimeException("unreachable");
                }
            }
        }
        if (!"m:NoSuchMethodException".equals(last)) {
            throw new RuntimeException("got " + last);
        }
        System.out.println("MultiCatchCommonSupertypeVerifyTest passed!");
    }
}
