/*
 * Regression test: a `continue` statement inside a for-loop (with an
 * update clause), a do-while loop, or the array form of an enhanced-for
 * loop - matching gumdrop's own DnsResolver.openBestTransport(), whose
 * "for (DnsTransportType type : TRANSPORT_PREFERENCE_ORDER) { ...
 * continue; ... }" (an enhanced-for over an array, with several `continue`
 * statements in its body) hits exactly this codegen path. Used to hang
 * the JVM in an infinite busy loop rather than throw any error - a
 * `UpstreamRelayHandlerDnssecTest` reproduction of the same shape burned
 * 100% CPU forever inside DnsResolver.openBestTransport(), confirmed via
 * jstack showing the main thread stuck RUNNABLE at that exact method with
 * no lock/IO wait involved.
 *
 * Root cause: for these three loop constructs, the real "continue target"
 * (the update expression for a for-loop, the condition re-check for a
 * do-while, the index-increment for an array-based enhanced-for) comes
 * AFTER the loop body, so its bytecode offset is only known once the body
 * has been fully generated - but codegen_stmt.c's AST_CONTINUE_STMT
 * computed and emitted its backward-branch offset immediately, using
 * whatever the loop's continue_target field held AT THAT MOMENT (still
 * mid-body). For these three constructs that field is only initialized to
 * loop_start (the condition/index check at the TOP of the loop, correct
 * for a while loop or the Iterable form of enhanced-for, which have no
 * separate update step) and only corrected to the real value in a
 * "update continue target" step that runs AFTER the whole body - too late
 * for any `continue` already generated partway through that same body.
 * The effect: `continue` jumped back to the top of the loop WITHOUT ever
 * reaching the update/increment step, so the loop's own progress variable
 * (array index, or a for-loop's own counter) never advanced past whatever
 * value the loop was on the first time `continue` fired - an infinite
 * loop re-trying that exact same iteration forever.
 *
 * Fixed by deferring every `continue`'s branch offset the same way
 * AST_BREAK_STMT already defers its own (forward) jump: emit a
 * placeholder goto and record its position on the target loop_context_t
 * (mg_add_continue_to_context()), then backpatch every recorded position
 * once the loop's real continue_target is finally known
 * (mg_patch_continue_offsets()), for every loop construct (while, do-
 * while, for, and both forms of enhanced-for) uniformly - restoring the
 * stackmap to the loop's pre-body local-variable snapshot first and
 * recording a stack map frame at that now-genuine branch target, exactly
 * like this codebase's existing loop-end/break-target frames already do.
 */
import java.util.ArrayList;
import java.util.List;

public class ContinueBackwardBranchTargetVerifyTest {

    public static void main(String[] args) {
        // Classic C-style for-loop with an update clause - continue must
        // reach "i++", not just loop back to the condition check.
        int sum1 = 0;
        for (int i = 0; i < 5; i++) {
            if (i == 2) {
                continue;
            }
            sum1 += i;
        }
        if (sum1 != 8) {
            throw new RuntimeException("expected sum1 == 8, got " + sum1);
        }

        // do-while - continue must reach the trailing condition check,
        // not loop back to the top of the body.
        int i2 = 0;
        int sum2 = 0;
        do {
            i2++;
            if (i2 == 2) {
                continue;
            }
            sum2 += i2;
        } while (i2 < 5);
        if (sum2 != 13) {
            throw new RuntimeException("expected sum2 == 13, got " + sum2);
        }

        // Enhanced-for over an array (the exact shape of gumdrop's own
        // DnsResolver.openBestTransport()'s "for (DnsTransportType type :
        // TRANSPORT_PREFERENCE_ORDER) { ... continue; ... }") - continue
        // must reach the index increment, not loop back to index 0.
        int[] arr = {0, 1, 2, 3, 4};
        int sum3 = 0;
        for (int v : arr) {
            if (v == 2) {
                continue;
            }
            sum3 += v;
        }
        if (sum3 != 8) {
            throw new RuntimeException("expected sum3 == 8, got " + sum3);
        }

        // Enhanced-for over an Iterable - already correct before this fix
        // (no separate update step), included here as a regression guard
        // on the refactor that unified all five loop constructs onto the
        // same deferred-patch mechanism.
        List<Integer> list = new ArrayList<Integer>();
        for (int v = 0; v < 5; v++) {
            list.add(Integer.valueOf(v));
        }
        int sum4 = 0;
        for (Integer v : list) {
            if (v.intValue() == 2) {
                continue;
            }
            sum4 += v.intValue();
        }
        if (sum4 != 8) {
            throw new RuntimeException("expected sum4 == 8, got " + sum4);
        }

        // Labeled continue on an outer for-loop from inside a nested
        // loop, matching LabeledStatements.java's own shape (never
        // actually executed by run-tests.sh, since it isn't a *Test.java
        // file - only compiled) - a labeled continue also needs its
        // target loop's real (post-body) continue target, not just an
        // unlabeled one reached from the innermost loop.
        int count = 0;
        outer:
        for (int a = 1; a <= 6; a++) {
            if (6 % a != 0) {
                continue outer;
            }
            for (int b = a + 1; b <= 6; b++) {
                if (6 % b == 0) {
                    count++;
                }
            }
        }
        if (count != 6) {
            throw new RuntimeException("expected count == 6, got " + count);
        }

        System.out.println("ContinueBackwardBranchTargetVerifyTest passed!");
    }
}
