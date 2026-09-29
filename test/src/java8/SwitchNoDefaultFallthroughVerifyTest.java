/*
 * Regression test: a plain `switch` statement (not a switch expression,
 * so exhaustiveness isn't required) over an enum, with NO explicit
 * "default:" label, where every explicit case ends in `return` - placed
 * inside a `try` block so something follows the switch (the enclosing
 * try/catch's own epilogue) - exactly gumdrop's own
 * SelectorLoop.isolateFailedHandler() shape - used to throw:
 *
 *   java.lang.VerifyError: Expecting a stackmap frame at branch target N
 *
 * Root cause: codegen_stmt.c's AST_SWITCH_STMT case only recorded a
 * stack-map frame at "switch_end" (the position right after the whole
 * switch statement) when some `break` statement actually targeted it
 * ("if (ctx->break_offsets) mg_record_frame(mg);"). But when there's no
 * explicit "default:" label, the lookupswitch/tableswitch's own default
 * offset is ALSO patched to point at exactly this same switch_end
 * position (the "no case matched" path falls through there) -
 * independent of whether any `break` exists at all. A plain switch
 * statement doesn't require exhaustiveness (unlike a switch expression),
 * so this is a perfectly ordinary, reachable path whenever the
 * selector's actual value doesn't match any case label - and even when
 * every case DOES return, the verifier still requires a stack-map frame
 * at any offset that a real bytecode branch (like the lookupswitch's own
 * default entry) can target. Fixed by also recording the frame whenever
 * there's no explicit default label, not just when a break exists.
 */
public class SwitchNoDefaultFallthroughVerifyTest {
    enum Kind { A, B, C }

    void handle(Kind k) {
        try {
            switch (k) {
                case A:
                    System.out.println("a");
                    return;
                case B:
                    System.out.println("b");
                    return;
                case C:
                    System.out.println("c");
                    return;
            }
        } catch (Exception e) {
            System.out.println("caught");
        }
        System.out.println("fell through");
    }

    public static void main(String[] args) {
        SwitchNoDefaultFallthroughVerifyTest t = new SwitchNoDefaultFallthroughVerifyTest();
        t.handle(Kind.A);
        t.handle(Kind.B);
        t.handle(Kind.C);
        System.out.println("SwitchNoDefaultFallthroughVerifyTest passed!");
    }
}
