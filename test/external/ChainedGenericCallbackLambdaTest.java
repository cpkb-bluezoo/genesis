import java.util.ArrayList;
import java.util.List;

import callbackslib.Safe;

/* A lambda passed to a method of a generic class-file type returned by a generic
 * call is typed by the arguments of that call ("Callback" here), not left as the
 * class's own type variable. */
public class ChainedGenericCallbackLambdaTest {
    interface Callback<B> {
        String name(B value);
    }

    static class Plain implements Callback<String> {
        public String name(String value) {
            return "plain-" + value;
        }
    }

    @SuppressWarnings({ "unchecked", "rawtypes" })
    public static void main(String[] args) {
        List<Callback<?>> callbacks = new ArrayList<Callback<?>>();
        callbacks.add(new Plain());
        final StringBuilder out = new StringBuilder();
        Safe.callbacks(Callback.class, callbacks, "x").invoke((callback) -> out.append(callback.name("a")));
        List<Runnable> runnables = new ArrayList<Runnable>();
        runnables.add(() -> out.append("+run"));
        Safe.callbacks(Runnable.class, runnables, "y").invoke((runnable) -> runnable.run());
        if (!"plain-a+run".equals(out.toString())) {
            throw new AssertionError(out.toString());
        }
        System.out.println("PASS");
    }
}
