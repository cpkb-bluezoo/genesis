/*
 * Regression test: a non-static inner class calling an OUTER class's
 * instance method with no explicit qualifier (e.g. "shutdown();" from
 * inside "private class ShutdownHook extends Thread { public void
 * run() { ...; shutdown(); } }", where "shutdown()" is declared on the
 * enclosing class, not on ShutdownHook itself or any of its
 * superclasses) - exactly gumdrop's own Gumdrop.ShutdownHook shape -
 * used to throw:
 *
 *   java.lang.VerifyError: Bad type on operand stack
 *   Reason: Type '...$Hook' (current frame, stack[0]) is not assignable
 *   to '...' [the outer class]
 *
 * Root cause: codegen_expr.c's codegen_method_call() correctly resolves
 * *which* method to invoke for an unqualified call (semantic analysis
 * already walks the enclosing-class chain and sets expr->sem_symbol
 * accordingly, so the invoke instruction's own method reference was
 * always right) - but the RECEIVER-loading code unconditionally emitted
 * a bare "aload_0" (this class's own "this") whenever the call wasn't
 * static, with no check for whether the resolved method actually
 * belongs to an ENCLOSING class rather than the current class or one of
 * its superclasses (an inherited method call correctly still uses plain
 * "this" - only an enclosing-instance call needs a different object).
 * Fixed by distinguishing the two cases (walking the current class's own
 * superclass chain to tell "inherited" from "enclosing") and, for the
 * enclosing case, loading the actual enclosing instance by walking the
 * this$0 chain (the same mechanism already used for "outer.new
 * Inner()"'s implicit outer-instance case), instead of a bare aload_0.
 */
public class InnerImplicitOuterMethodCallVerifyTest {
    private volatile boolean shutdownCalled;

    void shutdown() {
        shutdownCalled = true;
    }

    private class Hook extends Thread {
        Hook() {
            super("Hook");
        }

        @Override
        public void run() {
            shutdown();
        }
    }

    public static void main(String[] args) throws Exception {
        InnerImplicitOuterMethodCallVerifyTest t = new InnerImplicitOuterMethodCallVerifyTest();
        Thread h = t.new Hook();
        h.start();
        h.join();
        if (!t.shutdownCalled) {
            throw new RuntimeException("expected shutdown() to be called on the outer instance");
        }
        System.out.println("InnerImplicitOuterMethodCallVerifyTest passed!");
    }
}
