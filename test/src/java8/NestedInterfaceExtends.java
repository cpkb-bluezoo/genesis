/**
 * Anonymous class implements a nested interface that extends another nested
 * interface (same pattern as TlsHandshakeAsyncOffload.BatchProcessor).
 * Compile-only (not *Test.java) so run-tests.sh does not try to run main().
 */
public class NestedInterfaceExtends {

    static class Handshake {
        interface BatchProcessor {
            void process();
        }
    }

    static class Tls {
        interface BatchProcessor extends Handshake.BatchProcessor {
        }
    }

    void submit(Handshake.BatchProcessor processor) {
    }

    void test() {
        submit(new Tls.BatchProcessor() {
            @Override
            public void process() {
            }
        });
    }
}
