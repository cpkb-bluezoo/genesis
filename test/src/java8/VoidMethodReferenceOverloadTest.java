import java.util.concurrent.Callable;

/*
 * Regression test: "executor.submit(output::release)" for a method that returns void took the
 * Callable overload (BootstrapMethodError: "void is not convertible to Object"): a method
 * reference to a void method fits only an interface whose method returns void (JLS 15.13.2),
 * whether the overloads are submit(Runnable) and submit(Callable<T>) or others like them.
 */
public class VoidMethodReferenceOverloadTest {

    static String last = "";

    void release() {
        last = "release";
    }

    String value() {
        return "value";
    }

    static String submit(Callable<?> callable) throws Exception {
        return "callable:" + callable.call();
    }

    static String submit(Runnable runnable) {
        runnable.run();
        return "runnable";
    }

    public static void main(String[] args) throws Exception {
        VoidMethodReferenceOverloadTest test = new VoidMethodReferenceOverloadTest();
        String actual = submit(test::release) + " " + submit(test::value) + " " + last;
        if (!"runnable callable:value release".equals(actual)) {
            throw new RuntimeException("FAIL: " + actual);
        }
        System.out.println("PASS");
    }
}
