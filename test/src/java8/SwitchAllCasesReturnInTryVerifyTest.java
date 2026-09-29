/*
 * Regression test: a try body whose ONLY statement is a switch statement
 * where every case (including default) ends in `return` - matching
 * gumdrop's own DnssecValidator.buildPublicKey(), whose entire try body
 * is exactly such a switch over a DnssecAlgorithm enum. Used to throw at
 * class-verification time:
 *
 *   java.lang.VerifyError: Expecting a stack map frame
 *   Reason: Expected stackmap frame at this location.
 *
 * Root cause: AST_SWITCH_STMT's own codegen (codegen_stmt.c)
 * unconditionally reset mg->last_opcode to 0 after generating the whole
 * switch, on the theory that "switch doesn't guarantee method
 * termination" - true in general (a switch might have no default, or a
 * case might break/fall through past its end), but not when every case
 * (including a default) genuinely ends in return/throw. AST_TRY_STMT's
 * own "does the try body end with return" check (driving whether it
 * needs to append a dead "normal completion" goto after the body) relies
 * on mg->last_opcode - since the switch always reset it to 0, that check
 * always concluded the try body falls through, appending an unreachable
 * goto right after the switch's own dispatch code: dead code with no
 * stack map frame.
 *
 * Fixed by tracking, across the switch's own case-generation loop,
 * whether a `default` case is present and every case body (default
 * included) ends in return/throw specifically (not `break`, which
 * reaches "after the switch" the same as an ordinary fall-through
 * would) - and reflecting that in mg->last_opcode instead of always
 * resetting to 0.
 */
public class SwitchAllCasesReturnInTryVerifyTest {
    enum Algo { A, B, C, D, E, F }

    static String build(Algo algo) {
        try {
            switch (algo) {
                case A:
                case B:
                    return "AB";
                case C:
                case D:
                    return "CD";
                case E:
                case F:
                    return "EF";
                default:
                    return null;
            }
        } catch (Exception e) {
            return null;
        }
    }

    public static void main(String[] args) {
        if (!"AB".equals(build(Algo.A))) {
            throw new RuntimeException("expected AB for A, got " + build(Algo.A));
        }
        if (!"AB".equals(build(Algo.B))) {
            throw new RuntimeException("expected AB for B, got " + build(Algo.B));
        }
        if (!"CD".equals(build(Algo.C))) {
            throw new RuntimeException("expected CD for C, got " + build(Algo.C));
        }
        if (!"CD".equals(build(Algo.D))) {
            throw new RuntimeException("expected CD for D, got " + build(Algo.D));
        }
        if (!"EF".equals(build(Algo.E))) {
            throw new RuntimeException("expected EF for E, got " + build(Algo.E));
        }
        if (!"EF".equals(build(Algo.F))) {
            throw new RuntimeException("expected EF for F, got " + build(Algo.F));
        }

        System.out.println("SwitchAllCasesReturnInTryVerifyTest passed!");
    }
}
