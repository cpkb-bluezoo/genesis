import java.util.function.Supplier;

/**
 * Regression: block-body Supplier lambda (Flyway Extension ctor) must specialize get()
 * to the extension type, not Supplier or Object.
 */
public class LambdaSupplierBlockSamReturnVerifyTest {

    interface Ext {
    }

    static final class E implements Ext {
    }

    static Ext load(Ext e) {
        return e;
    }

    static Supplier<Ext> supplier() {
        return () -> {
            Ext extension = new E();
            return extension;
        };
    }

    public static void main(String[] args) {
        if (!(supplier().get() instanceof E)) {
            throw new RuntimeException("bad supplier return");
        }
        System.out.println("PASS");
    }
}
