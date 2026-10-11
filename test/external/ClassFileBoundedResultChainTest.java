import bres.Registry;

/*
 * Regression test: "registry.getAppender(n).getLayout()" for a class file's
 * "<T extends Appender> T getAppender(String)", in a position with a target type (a return of
 * Object): T was settled by that target (Object) instead of its own bound, so getLayout was not
 * found and the call went to the wrong class (VerifyError).
 */
public class ClassFileBoundedResultChainTest {

    static Object layout(Registry registry) {
        return registry.getAppender("console").getLayout();
    }

    static String pattern(Registry registry) {
        return registry.getAppender("console").getLayout().pattern();
    }

    public static void main(String[] args) {
        Registry registry = new Registry() {
            @Override
            @SuppressWarnings("unchecked")
            public <T extends Appender> T getAppender(String name) {
                return (T) (Appender) () -> () -> "%p";
            }
        };
        if (layout(registry) == null || !"%p".equals(pattern(registry))) {
            throw new RuntimeException("FAIL");
        }
        System.out.println("PASS");
    }
}
