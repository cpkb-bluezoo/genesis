/*
 * codegen_load_enclosing_this() (codegen_expr.c) walks the "this$0" chain
 * from an inner/anonymous class out to an enclosing instance, one GETFIELD
 * per hop. Each GETFIELD genuinely changes the value's type on the real
 * operand stack (inner class -> its enclosing class), but the helper never
 * updated mg->stackmap's own tracked TYPE for that slot to match - it
 * stayed at whatever the inner class type was from the initial ALOAD_0.
 * Invisible when the loaded enclosing instance is consumed immediately
 * (e.g. as a direct, sole method-call receiver), but a real bug the
 * moment a stackmap frame is recorded while it's still on the stack
 * UNDERNEATH something else being evaluated - e.g. as the receiver of an
 * outer method call whose argument is itself a ternary, whose own
 * branches each get their own recorded frame: VerifyError "Type
 * Outer ... is not assignable to Outer$1".
 *
 * Confirmed against gumdrop's own Pop3ProtocolHandler's RETR-offload
 * failure callback: "recordSessionException(error instanceof Exception ?
 * (Exception) error : new Exception(error))" called unqualified (i.e. on
 * the enclosing Pop3ProtocolHandler instance) from within an anonymous
 * StorageExecutor.Callback.
 */
public class EnclosingThisTernaryArgVerifyTest {
    interface Callback {
        void failed(Throwable t);
    }

    void record(Exception e) {
        System.out.println("recorded: " + e.getMessage());
    }

    Callback makeCallback() {
        return new Callback() {
            @Override
            public void failed(Throwable error) {
                record(error instanceof Exception
                        ? (Exception) error
                        : new Exception(error));
            }
        };
    }

    public static void main(String[] args) {
        EnclosingThisTernaryArgVerifyTest outer = new EnclosingThisTernaryArgVerifyTest();
        Callback cb = outer.makeCallback();
        cb.failed(new RuntimeException("boom"));
        cb.failed(new Error("bad"));
        System.out.println("EnclosingThisTernaryArgVerifyTest passed!");
    }
}
