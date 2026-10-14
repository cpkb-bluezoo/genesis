import lambdalog.Logger;
import lambdalog.Message;
import lambdalog.MessageSupplier;
import lambdalog.Supplier;

public class LambdaOverloadInheritedSamTest {
    static String last;

    static class Recorder implements Logger {
        public void debug(Message message) {
            last = "message";
        }

        public void debug(MessageSupplier supplier) {
            last = "message-supplier";
        }

        public void debug(CharSequence message) {
            last = "char-sequence";
        }

        public void debug(Object message) {
            last = "object";
        }

        public void debug(String message) {
            last = "string";
        }

        public void debug(Supplier<?> supplier) {
            last = "supplier";
        }
    }

    // The sole abstract method of MessageSupplier returns Message, redeclared over a
    // generic superinterface's "T get()": a String-valued lambda is no MessageSupplier.
    public static void main(String[] args) {
        Logger log = new Recorder();
        log.debug(() -> "hello");
        if (!"supplier".equals(last)) throw new AssertionError(last);
        log.debug(() -> new Message() {
            public String getFormattedMessage() {
                return "m";
            }

            public Object[] getParameters() {
                return null;
            }
        });
        if (!"message-supplier".equals(last)) throw new AssertionError(last);
        System.out.println("ok");
    }
}
