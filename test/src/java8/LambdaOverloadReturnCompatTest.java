import java.util.function.Supplier;

public class LambdaOverloadReturnCompatTest {
    interface Message {
        String text();

        int id();
    }

    interface MessageSupplier {
        Message get();
    }

    static class Log {
        String last;

        void debug(String msg) {
            last = "string";
        }

        void debug(Message msg) {
            last = "message";
        }

        void debug(MessageSupplier supplier) {
            last = "message-supplier";
        }

        void debug(Supplier<?> supplier) {
            last = "supplier";
        }
    }

    public static void main(String[] args) {
        Log log = new Log();
        log.debug(() -> "hello");
        if (!"supplier".equals(log.last)) throw new AssertionError(log.last);
        log.debug(() -> new Message() {
            public String text() {
                return "m";
            }

            public int id() {
                return 1;
            }
        });
        if (!"message-supplier".equals(log.last)) throw new AssertionError(log.last);
        System.out.println("ok");
    }
}
