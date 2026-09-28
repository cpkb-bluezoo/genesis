/*
 * Regression test: a synchronized block that assigns a local declared
 * before it (or partway through it) and has a live normal-exit path, where
 * an exception could also occur, used to produce a class file that failed
 * JVM bytecode verification with:
 *
 *   java.lang.VerifyError: Stack map does not match the one at exception
 *   handler N
 *   Reason: Type top (current frame, locals[k]) is not assignable to
 *   '...' (stack map, locals[k])
 *
 * codegen_stmt.c's AST_SYNCHRONIZED_STMT records its exception handler's
 * stack-map frame using mg->stackmap's *current* state at that point in
 * codegen - which by then reflects everything the body assigned (since an
 * exception handler is recorded right after generating the body). But the
 * handler's protected range starts at the top of the synchronized block,
 * before any of that ran - an exception thrown that early still reaches
 * the handler, and at that point those locals are genuinely uninitialized.
 * This is the exact same mistake already fixed for AST_TRY_STMT's
 * try_entry_state (see TryFinallyLocalVerifyTest.java) and the finally
 * handler bug before it, just in a third, independent code path.
 *
 * Fixing the handler's own frame introduces a second, related risk: if it
 * restores mg->stackmap to the block's *entry* state before framing the
 * handler, that restored (entry) state is what's left behind afterward -
 * so the "after handler" join point (reachable only via the body's own
 * normal-exit goto, since the handler always rethrows) would then be
 * framed from the wrong (entry) state too, unless a *second* snapshot is
 * taken of the body's actual exit state and restored specifically for that
 * join point. This test's getLive() exercises exactly that: a live normal
 * exit *and* a possible exception, both after a local assignment inside
 * the block.
 * See genesis history for details (search "sync_entry_state" and
 * "sync_body_exit_state" in codegen_stmt.c).
 */
public class SyncHandlerEntryStateVerifyTest {
    private final Object lock = new Object();

    static class Missing extends RuntimeException {
        Missing(String s) { super(s); }
    }

    String locate(String key) {
        if (key.equals("missing")) {
            throw new Missing(key);
        }
        return key.toUpperCase();
    }

    String get(String key, boolean simple) {
        synchronized (lock) {
            if (simple) {
                return "SIMPLE";
            }
            String found = locate(key);
            return found + "!";
        }
    }

    String getLive(String key) {
        String result;
        synchronized (lock) {
            result = locate(key);
        }
        return result.toLowerCase();
    }

    public static void main(String[] args) {
        SyncHandlerEntryStateVerifyTest t = new SyncHandlerEntryStateVerifyTest();
        if (!"SIMPLE".equals(t.get("x", true))) {
            throw new RuntimeException("simple branch failed");
        }
        if (!"ABC!".equals(t.get("abc", false))) {
            throw new RuntimeException("normal branch failed");
        }
        try {
            t.get("missing", false);
            throw new RuntimeException("expected Missing to propagate");
        } catch (Missing expected) {
            /* expected */
        }
        if (!"abc".equals(t.getLive("abc"))) {
            throw new RuntimeException("getLive normal path failed");
        }
        try {
            t.getLive("missing");
            throw new RuntimeException("expected Missing to propagate from getLive");
        } catch (Missing expected) {
            /* expected */
        }
        System.out.println("All tests passed!");
    }
}
