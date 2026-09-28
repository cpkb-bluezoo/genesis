/*
 * Regression test: an anonymous class nested inside *another* anonymous
 * class's method body, referencing a variable whose true home scope is
 * further out than the immediately-enclosing class's own enclosing method
 * (e.g. a parameter of the method the *outer* anonymous class is itself
 * defined in, which the outer class's own body never happens to reference
 * directly) - used to produce a classfile that failed JVM bytecode
 * verification:
 *
 *   java.lang.VerifyError: Bad type on operand stack
 *   Reason: Type '<OuterAnonClass>' (current frame, stack[k]) is not
 *   assignable to '<SomeCapturedType>'
 *
 * Two independent gaps combined to cause this:
 *
 * 1. semantic.c's per-identifier capture detection (in get_expression_type()'s
 *    AST_IDENTIFIER case) only ever added a captured variable to the
 *    *innermost* local/anonymous class actually referencing it - it never
 *    asked whether any *enclosing* local/anonymous class between that
 *    variable's true home scope and the innermost class also needed to
 *    capture it, purely to relay it down through its own constructor. So
 *    the outer anonymous class's own captured-variable list (and therefore
 *    its constructor's own parameter list) silently omitted a variable it
 *    never itself referenced, even though its nested class's constructor
 *    needed it passed through. Fixed by propagate_capture_to_enclosing_classes(),
 *    which walks the enclosing_class chain adding the same capture to each
 *    ancestor that doesn't already have direct access to it.
 *
 * 2. Even with the outer class's own capture list fixed, codegen_expr.c's
 *    "push captured variable values" loop (at the `new InnerClass(...)`
 *    call site, inside the outer class's own method body) only ever
 *    checked mg->locals (a plain local or parameter of the *current*
 *    method) for each captured variable - never mg->class_gen's own field
 *    map, even though the outer class had, thanks to fix 1, now captured
 *    that same variable itself, making it available as this.val$<name>
 *    instead of a plain local. Without a fallback there, the argument was
 *    silently dropped from the constructor call entirely, so the actual
 *    bytecode pushed fewer arguments than the constructor's own descriptor
 *    declared - a stack-shape mismatch the verifier rejects. Fixed by
 *    falling back to a getfield load from this.val$<name> when the
 *    captured variable isn't found as a plain local.
 */
public class TransitiveCaptureVerifyTest {
    interface Callback<T> {
        void succeeded(T result);
        void failed(Throwable error);
    }

    <T> void submit(final java.util.concurrent.Executor loopDispatcher,
                     final java.util.concurrent.Callable<T> operation,
                     final Callback<T> callback) {
        final Runnable task = new Runnable() {
            @Override
            public void run() {
                T result = null;
                Throwable error = null;
                try {
                    result = operation.call();
                } catch (Throwable t) {
                    error = t;
                }
                final T finalResult = result;
                final Throwable finalError = error;
                loopDispatcher.execute(new Runnable() {
                    @Override
                    public void run() {
                        if (finalError != null) {
                            callback.failed(finalError);
                        } else {
                            callback.succeeded(finalResult);
                        }
                    }
                });
            }
        };
        task.run();
    }

    public static void main(String[] args) {
        TransitiveCaptureVerifyTest t = new TransitiveCaptureVerifyTest();
        final boolean[] succeeded = { false };
        t.submit(new java.util.concurrent.Executor() {
            public void execute(Runnable r) {
                r.run();
            }
        }, new java.util.concurrent.Callable<String>() {
            public String call() {
                return "hello";
            }
        }, new Callback<String>() {
            public void succeeded(String result) {
                if (!"hello".equals(result)) {
                    throw new RuntimeException("expected hello, got " + result);
                }
                succeeded[0] = true;
            }
            public void failed(Throwable error) {
                throw new RuntimeException("unexpected failure", error);
            }
        });
        if (!succeeded[0]) {
            throw new RuntimeException("callback did not run");
        }

        final boolean[] failed = { false };
        t.submit(new java.util.concurrent.Executor() {
            public void execute(Runnable r) {
                r.run();
            }
        }, new java.util.concurrent.Callable<String>() {
            public String call() throws Exception {
                throw new java.io.IOException("boom");
            }
        }, new Callback<String>() {
            public void succeeded(String result) {
                throw new RuntimeException("expected failure, got success: " + result);
            }
            public void failed(Throwable error) {
                if (!"boom".equals(error.getMessage())) {
                    throw new RuntimeException("wrong error message: " + error.getMessage());
                }
                failed[0] = true;
            }
        });
        if (!failed[0]) {
            throw new RuntimeException("failure callback did not run");
        }

        System.out.println("All tests passed!");
    }
}
