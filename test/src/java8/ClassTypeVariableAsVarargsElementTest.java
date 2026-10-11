/*
 * Regression test: an argument at the varargs position whose type is a class type variable
 * ("get()" returning C, passed to format(String, Object...)) was typed by the ARRAY parameter
 * as the target, so a cast to Object[] was made on it (ClassCastException at run time).
 */
public class ClassTypeVariableAsVarargsElementTest {

    static class Holder<C extends Runnable> {
        private final C value;

        Holder(C value) {
            this.value = value;
        }

        protected final C get() {
            return value;
        }

        String describe(Class<?> type) {
            return format("%s from %s", type.getSimpleName(), get());
        }
    }

    static String format(String pattern, Object... args) {
        return args.length + ":" + String.format(pattern, args);
    }

    public static void main(String[] args) {
        Runnable task = new Runnable() {
            public void run() {
            }

            public String toString() {
                return "task";
            }
        };
        String actual = new Holder<Runnable>(task).describe(String.class);
        if (!"2:String from task".equals(actual)) {
            throw new RuntimeException("FAIL: " + actual);
        }
        System.out.println("ClassTypeVariableAsVarargsElementTest passed!");
    }
}
