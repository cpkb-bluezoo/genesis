/*
 * Regression test: a block-bodied lambda (a lambda whose body is `{ ... }`,
 * not a single expression) that references a captured local variable
 * anywhere other than inside a `return` statement used to fail codegen
 * entirely:
 *
 *   codegen: cannot resolve identifier: <name>
 *
 * bind_lambda_to_target_type() (src/semantic.c) is responsible for both
 * type-checking a lambda's body and detecting which enclosing locals it
 * captures - captures are recorded as a side effect of resolving each
 * identifier via get_expression_type()'s AST_IDENTIFIER case. For an
 * expression-bodied lambda, the whole body expression is run through
 * get_expression_type(), so every identifier in it - and thus every
 * capture - gets found. For a block-bodied lambda, only two narrow scans
 * ran: one purely for 'this' capture (scan_lambda_body_for_this_capture),
 * and, only when the SAM's return type needed inference, a scan of return
 * statements' expressions. Nothing walked the rest of the block, so a
 * capture used only in a non-return statement (an assignment, a bare
 * expression statement, inside a synchronized block, etc.) never made it
 * into lambda_captures - and codegen, which only knows about captures
 * listed there, had no local variable slot to resolve the reference
 * against.
 * See genesis history for details (search "local variable or parameter
 * captured" in scan_lambda_body_for_this_capture, src/semantic.c).
 */
public class LambdaBlockBodyCaptureTest {
    Object lock = new Object();

    public static void main(String[] args) throws Exception {
        // Capture used as a synchronized-block target and read inside it -
        // the exact shape that originally surfaced this bug.
        LambdaBlockBodyCaptureTest owner = new LambdaBlockBodyCaptureTest();
        final boolean[] acquired = { false };
        Thread other = new Thread(() -> {
            synchronized (owner.lock) {
                acquired[0] = true;
            }
        });
        other.start();
        other.join(5000);
        if (!acquired[0]) {
            throw new RuntimeException("synchronized-block capture failed");
        }

        // Capture used only as a bare statement (no synchronized, no field
        // access) inside a block body.
        String prefix = "Result: ";
        Runnable printer = () -> {
            System.out.println(prefix + "42");
        };
        printer.run();

        // Capture used only in a local variable's initializer inside a
        // block body (never appears in a return statement).
        Object value = new Object();
        Runnable reader = () -> {
            Object x = value;
            if (x != value) {
                throw new RuntimeException("re-read of captured local mismatched");
            }
        };
        reader.run();

        System.out.println("All tests passed!");
    }
}
