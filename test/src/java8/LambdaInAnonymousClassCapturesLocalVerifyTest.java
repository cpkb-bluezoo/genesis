import java.util.concurrent.CountDownLatch;

/**
 * Regression: a lambda inside an anonymous class method must load captured
 * outer locals from val$ fields when pushing invokedynamic bootstrap args.
 * Loading slot 0 (this) instead caused VerifyError in HeapDumpWebEndpointTests.
 */
public class LambdaInAnonymousClassCapturesLocalVerifyTest {
    interface Factory {
        Runnable make();
    }

    static boolean run(CountDownLatch signal, CountDownLatch gate) throws InterruptedException {
        Factory f = new Factory() {
            @Override
            public Runnable make() {
                return () -> {
                    signal.countDown();
                    gate.await();
                };
            }
        };
        Runnable r = f.make();
        Thread t = new Thread(r);
        t.start();
        signal.await();
        gate.countDown();
        t.join();
        return true;
    }

    public static void main(String[] args) throws Exception {
        if (!run(new CountDownLatch(1), new CountDownLatch(1))) {
            throw new RuntimeException("lambda in anonymous class");
        }
        System.out.println("LambdaInAnonymousClassCapturesLocalVerifyTest passed!");
    }
}
