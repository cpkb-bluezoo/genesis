/**
 * Bug: incrementing/decrementing a WIDE (long/double) field of an
 * ENCLOSING class, accessed via the this$0 chain from a nested
 * (possibly doubly-nested) anonymous class - e.g. "failedAuthAttempts++"
 * where failedAuthAttempts is a long field of the outermost class,
 * referenced from an anonymous callback two levels of anonymous nesting
 * away - unconditionally used the int-only opcodes (ICONST_1/IADD/
 * DUP_X1) regardless of the field's real type. A long/double value needs
 * 2 operand-stack slots; DUP_X1 only handles a single (category-1) slot,
 * so the verifier rejects it outright. VerifyError: "Bad type on
 * operand stack ... Type long_2nd ... not assignable to category1 type".
 * This is a sibling of the already-fixed "obj.field++ on a wide field"
 * bug, in a SEPARATE codegen branch (the "field belongs to an enclosing
 * class, accessed via this$0" case) that never got the same fix.
 * Confirmed against gumdrop's own Pop3ProtocolHandler, whose
 * "failedAuthAttempts++" (a long field, incremented from a doubly-nested
 * anonymous StorageExecutor.Callback) is exactly this shape.
 */
public class WideEnclosingFieldIncDecVerifyTest {
    long failedAuthAttempts;
    double totalScore;

    interface Callback {
        void completed(Boolean b);
    }

    interface Outer1 {
        void run();
    }

    void submitOuter(Outer1 r) {
        r.run();
    }

    void submitInner(Callback cb) {
        cb.completed(Boolean.FALSE);
    }

    void run() {
        submitOuter(new Outer1() {
            @Override
            public void run() {
                submitInner(new Callback() {
                    @Override
                    public void completed(Boolean authenticated) {
                        failedAuthAttempts++;
                        long pre = ++failedAuthAttempts;
                        if (pre != failedAuthAttempts) {
                            throw new RuntimeException("pre-increment mismatch");
                        }
                        totalScore--;
                    }
                });
            }
        });
    }

    public static void main(String[] args) {
        WideEnclosingFieldIncDecVerifyTest t = new WideEnclosingFieldIncDecVerifyTest();
        t.run();
        if (t.failedAuthAttempts != 2) {
            throw new RuntimeException("expected failedAuthAttempts=2, got " + t.failedAuthAttempts);
        }
        if (t.totalScore != -1.0) {
            throw new RuntimeException("expected totalScore=-1.0, got " + t.totalScore);
        }
    }
}
