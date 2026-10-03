/* Regression test: try { synchronized (lock) { T x = ...; if (x == null)
 * return; ... } } finally { T x = ...; if (x != null) ... } - an early
 * return inside a synchronized block inside a try-finally whose finally
 * body declares its own local of the same name. The finally body, inlined
 * at the return path, read a local slot the verifier saw as "top"
 * ("Bad local variable type"). Mirrors gumdrop's TlsRecordState.unwrap(). The
 * finally's own "in" (declared while inlining it at the early return) leaked
 * into the name->slot map, so the LATER use of the synchronized body's "in"
 * after that return resolved to the finally's slot. */
public class SyncReturnFinallyLocalVerifyTest {
    private final Object lock = new Object();
    private StringBuilder buf;
    int compacted;
    int fed;

    StringBuilder netIn() {
        return buf;
    }

    void unwrap() {
        try {
            synchronized (lock) {
                StringBuilder in = netIn();
                if (in == null) {
                    return;
                }
                fed += in.length() + 1;
            }
        } finally {
            StringBuilder in = netIn();
            if (in != null) {
                compacted++;
            }
        }
    }

    public static void main(String[] args) {
        SyncReturnFinallyLocalVerifyTest t = new SyncReturnFinallyLocalVerifyTest();
        t.unwrap();
        if (t.fed != 0 || t.compacted != 0) {
            throw new RuntimeException("null path: " + t.fed + "," + t.compacted);
        }
        t.buf = new StringBuilder("ab");
        t.unwrap();
        if (t.fed != 3 || t.compacted != 1) {
            throw new RuntimeException("buffer path: " + t.fed + "," + t.compacted);
        }
        System.out.println("SyncReturnFinallyLocalVerifyTest passed!");
    }
}
