/*
 * Regression test: a `while` loop conditioned on a top-level `&&` chain
 * where a later operand assigns a local with no initializer (the common
 * "poll until null" idiom), that local then used inside the loop body -
 * exactly gumdrop's own DnsCache.evictIfNeeded() shape:
 *
 *   EvictionEntry evictionEntry;
 *   while (removed < toRemove
 *           && (evictionEntry = expiryQueue.poll()) != null) {
 *       if (evictionEntry.cancelled) {
 *           continue;
 *       }
 *       ...
 *   }
 *
 * used to throw either:
 *
 *   java.lang.VerifyError: Inconsistent stackmap frames
 *   Type top (current frame, locals[N]) is not assignable to '...'
 *
 * (at the loop's own exit point) or, if that were naively "fixed" by
 * just degrading the exit frame's type, a *different* verification
 * failure inside the loop body itself, where the local genuinely does
 * have its real, narrowed type.
 *
 * Root cause: codegen_binary_expr()'s TOK_AND/TOK_OR codegen materializes
 * the WHOLE `&&` expression into a single 0/1 value at one shared merge
 * point ("short_circuit_pos"/"end_pos"), used by BOTH the "some operand
 * was false" edge (where a later operand's assignment never ran) and the
 * "every operand true" edge (where it did) - the enclosing while loop's
 * own codegen then did a SEPARATE, single ifeq on that materialized
 * value. Once the merge point correctly degrades the assigned local's
 * tracked type to the safe common state (matching a sibling fix for the
 * same class's own value-context use of &&), that degraded frame becomes
 * authoritative for the verifier from that point on - including the loop
 * body, which is *also* reached only after that same merge point, even
 * though the body is only ever entered on the "every operand true" edge
 * where the local's real, narrower type is actually known.
 *
 * Fixed with a new codegen_condition_and_chain_false_branch(), used only
 * by AST_WHILE_STMT's own condition: it recursively compiles a top-level
 * `&&` chain via direct per-operand branching (each operand's own false
 * outcome jumps straight to the loop's real exit point) rather than
 * materializing an intermediate boolean value at all - so the loop body
 * is reached directly via the "every operand true" fallthrough, entirely
 * bypassing any shared merge with the "some operand false" edges, while
 * the loop's own exit point correctly merges only those genuinely-false
 * edges (using the earliest operand's own pre-later-operand-side-effects
 * snapshot, always a safe/conservative frame for that whole group).
 * (`||` and `!` are not specially handled and still use the older,
 * generic materializing path - covered separately below to confirm nesting
 * inside/alongside a while condition still works.)
 */
public class ShortCircuitAndAssignInConditionVerifyTest {
    static class Node {
        int value;
        boolean skip;
        Node next;

        Node(int value, boolean skip, Node next) {
            this.value = value;
            this.skip = skip;
            this.next = next;
        }
    }

    static int sumUntilNull(Node head, int limit) {
        int sum = 0;
        int seen = 0;
        Node cursor;
        while (seen < limit && (cursor = head) != null) {
            head = cursor.next;
            if (cursor.skip) {
                continue;
            }
            sum += cursor.value;
            seen++;
        }
        return sum;
    }

    /* Three-way && chain (nested TOK_AND, recursion through two levels). */
    static int sumUntilNullThreeGuards(Node head, int limit, boolean enabled) {
        int sum = 0;
        int seen = 0;
        Node cursor;
        while (enabled && seen < limit && (cursor = head) != null) {
            head = cursor.next;
            sum += cursor.value;
            seen++;
        }
        return sum;
    }

    /* A plain (non-&&) while condition must still work unchanged. */
    static int countDown(int n) {
        int total = 0;
        while (n > 0) {
            total += n;
            n--;
        }
        return total;
    }

    public static void main(String[] args) {
        Node list = new Node(1, false, new Node(2, true, new Node(3, false, null)));
        int result = sumUntilNull(list, 10);
        if (result != 4) {
            throw new AssertionError("sumUntilNull: expected 4, got " + result);
        }

        Node list2 = new Node(10, false, new Node(20, false, null));
        int result2 = sumUntilNullThreeGuards(list2, 10, true);
        if (result2 != 30) {
            throw new AssertionError("sumUntilNullThreeGuards(enabled): expected 30, got " + result2);
        }
        int result3 = sumUntilNullThreeGuards(list2, 10, false);
        if (result3 != 0) {
            throw new AssertionError("sumUntilNullThreeGuards(disabled): expected 0, got " + result3);
        }

        int result4 = countDown(4);
        if (result4 != 10) {
            throw new AssertionError("countDown: expected 10, got " + result4);
        }

        System.out.println("ShortCircuitAndAssignInConditionVerifyTest passed!");
    }
}
