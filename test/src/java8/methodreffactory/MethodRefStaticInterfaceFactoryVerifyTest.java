package methodreffactory;

/**
 * Regression: static interface factory method references must use the method's
 * real return type in the bootstrap MethodHandle, not ()V when the declaring
 * type was compiled in another source file of the batch.
 */
public class MethodRefStaticInterfaceFactoryVerifyTest {

    interface SupplierLike<T> {
        T get();
    }

    static final class Factory {
        static final SupplierLike<ClientHttpRequestFactoryBuilder> HTTP =
            ClientHttpRequestFactoryBuilder::httpComponents;

        static void run() {
            if (HTTP.get() == null) {
                throw new RuntimeException("null builder");
            }
        }
    }

    public static void main(String[] args) {
        Factory.run();
        System.out.println("PASS");
    }
}
