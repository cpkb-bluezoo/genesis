import lambdalog.Logger;
import lambdalog.Message;
import lambdalog.MessageSupplier;
import lambdalog.Supplier;

public class LambdaOverloadStringCallBodyTest {
    static String last;

    static class Recorder implements Logger {
        public void debug(Message message) { last = "message"; }
        public void debug(MessageSupplier supplier) { last = "message-supplier"; }
        public void debug(CharSequence message) { last = "char-sequence"; }
        public void debug(Object message) { last = "object"; }
        public void debug(String message) { last = "string"; }
        public void debug(Supplier<?> supplier) { last = "supplier"; }
    }

    static String format() {
        return "hello";
    }

    String describe() {
        return "this";
    }

    static <T> T pick(T value) {
        return value;
    }

    // A zero-parameter lambda is explicitly typed, so its body takes part in applicability:
    // a call returning String is a Supplier<?> but no MessageSupplier (JLS 15.12.2.2).
    public static void main(String[] args) {
        Logger log = new Recorder();
        log.debug(() -> format());
        if (!"supplier".equals(last)) throw new AssertionError(last);
        log.debug(() -> new LambdaOverloadStringCallBodyTest().describe());
        if (!"supplier".equals(last)) throw new AssertionError(last);
        log.debug(() -> String.valueOf(1));
        if (!"supplier".equals(last)) throw new AssertionError(last);
        // a generic method's result adapts to the target, so it stays a MessageSupplier
        Message m = new Message() {
            public String getFormattedMessage() { return "m"; }
            public Object[] getParameters() { return null; }
        };
        log.debug(() -> pick(m));
        if (!"message-supplier".equals(last)) throw new AssertionError(last);
        System.out.println("ok");
    }
}
