import java.util.function.Supplier;

/*
 * Regression test: "this.properties.getTransaction()::isEnabled" (a bound method reference whose
 * receiver is a call on a field of this) compiled the call of getTransaction() as one on the
 * current class: VerifyError at class load.
 */
public class BoundMethodRefOnFieldCallReceiverTest {

    static class Transaction {
        boolean isEnabled() {
            return true;
        }
    }

    static class Properties {
        Transaction getTransaction() {
            return new Transaction();
        }
    }

    private final Properties properties = new Properties();

    boolean run() {
        Supplier<Boolean> enabled = this.properties.getTransaction()::isEnabled;
        return enabled.get();
    }

    public static void main(String[] args) {
        if (!new BoundMethodRefOnFieldCallReceiverTest().run()) {
            throw new RuntimeException("FAIL");
        }
        System.out.println("BoundMethodRefOnFieldCallReceiverTest passed!");
    }
}
