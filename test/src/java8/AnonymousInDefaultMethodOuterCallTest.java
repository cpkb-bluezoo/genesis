import java.util.concurrent.Callable;

/* Regression test: an anonymous class in an interface default method that
 * calls another method of the interface on the enclosing instance. The
 * anonymous class needs the outer instance (this$0): its constructor took
 * only the captured locals while the creation site passed the outer
 * instance too, so the class failed with NoSuchMethodError. Mirrors
 * gumdrop's SynchronousRealm. */
public class AnonymousInDefaultMethodOuterCallTest {
    interface Checker {
        boolean matches(String user, String password);

        default Callable<Boolean> deferred(final String user, final String password) {
            return new Callable<Boolean>() {
                @Override
                public Boolean call() {
                    return Boolean.valueOf(matches(user, password));
                }
            };
        }

        default Callable<String> describe() {
            return new Callable<String>() {
                @Override
                public String call() {
                    return "checker:" + matches("a", "b");
                }
            };
        }
    }

    public static void main(String[] args) throws Exception {
        Checker c = new Checker() {
            public boolean matches(String user, String password) {
                return user.equals(password);
            }
        };
        if (!c.deferred("x", "x").call().booleanValue()) {
            throw new RuntimeException("deferred x/x");
        }
        if (c.deferred("x", "y").call().booleanValue()) {
            throw new RuntimeException("deferred x/y");
        }
        if (!"checker:false".equals(c.describe().call())) {
            throw new RuntimeException(c.describe().call());
        }
        System.out.println("AnonymousInDefaultMethodOuterCallTest passed!");
    }
}
