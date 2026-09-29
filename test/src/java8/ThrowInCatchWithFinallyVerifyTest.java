/*
 * Regression test: a try/catch/finally whose catch clause's LAST
 * statement is itself a `throw` (e.g. wrapping the caught exception and
 * re-throwing it) - matching gumdrop's own BasicRealm.setHref(String),
 * which does:
 *
 *   try {
 *       ...
 *   } catch (IOException | SAXException e) {
 *       throw new RuntimeException("Failed to parse realm configuration: "
 *           + href, e);
 *   } finally {
 *       currentGroupName = null;
 *       pendingGroupRefs = null;
 *   }
 *
 * - used to throw at class-verification time:
 *
 *   java.lang.VerifyError: Expecting a stack map frame
 *   Reason: Expected stackmap frame at this location.
 *
 * Root cause: AST_TRY_STMT's catch-clause codegen unconditionally
 * appended a second, inlined copy of the finally block's own bytecode
 * right after each catch clause's body, regardless of whether that catch
 * body already ended with an unconditional return/throw. A `throw` as
 * the catch body's last statement emits ATHROW directly; the catch
 * clause's own bytecode range is already registered as protected by an
 * "any -> finally handler" exception table entry, so the finally block
 * already runs correctly via the JVM's own exception dispatch when the
 * throw propagates - appending a second copy directly after the ATHROW
 * is dead code, and (being the instruction immediately following an
 * unconditional branch) invalid without a stack map frame nothing
 * records for it. The try body's own equivalent case (already fixed,
 * see try_body_ends_with_return) skips its own redundant copy the same
 * way; this fixes the catch clause's identical, previously-unfixed
 * sibling case.
 */
public class ThrowInCatchWithFinallyVerifyTest {
    private String state;

    void run(boolean fail) {
        try {
            if (fail) {
                throw new RuntimeException("boom");
            }
            state = "try-completed";
        } catch (RuntimeException e) {
            throw new IllegalStateException("wrapped: " + e.getMessage(), e);
        } finally {
            state = "finally-ran";
        }
    }

    public static void main(String[] args) {
        ThrowInCatchWithFinallyVerifyTest t = new ThrowInCatchWithFinallyVerifyTest();

        boolean caught = false;
        try {
            t.run(true);
        } catch (IllegalStateException e) {
            caught = true;
            if (!"wrapped: boom".equals(e.getMessage())) {
                throw new RuntimeException("expected 'wrapped: boom', got " + e.getMessage());
            }
            if (!(e.getCause() instanceof RuntimeException) || !"boom".equals(e.getCause().getMessage())) {
                throw new RuntimeException("expected cause 'boom', got " + e.getCause());
            }
        }
        if (!caught) {
            throw new RuntimeException("expected IllegalStateException to propagate");
        }
        if (!"finally-ran".equals(t.state)) {
            throw new RuntimeException("expected finally to have run, state=" + t.state);
        }

        t.run(false);
        if (!"finally-ran".equals(t.state)) {
            throw new RuntimeException("expected finally to have run on success path, state=" + t.state);
        }

        System.out.println("ThrowInCatchWithFinallyVerifyTest passed!");
    }
}
