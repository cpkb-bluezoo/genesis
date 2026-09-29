/*
 * Regression test: a scalar (int) array-element load (`arr[i]`) used as one
 * branch of a ternary expression whose result crosses a stack-map merge
 * point (here, as a constructor/method-call argument) - preceded by an
 * unrelated reference-typed ternary over the same array, matching
 * gumdrop's own DnsResolver.query() shape:
 *
 *   int[] plan = buildPlan();
 *   Object targetServer = plan.length == 0 ? null : pick(plan);
 *   consume(1, plan.length == 0 ? 0 : plan[0], 3);
 *
 * used to throw:
 *
 *   java.lang.VerifyError: Inconsistent stackmap frames
 *   Type integer (current frame, stack[N]) is not assignable to '[I'
 *
 * Root cause: codegen_expr.c's AST_ARRAY_ACCESS codegen emits the load
 * opcode (e.g. IALOAD, consuming the arrayref+index pair and producing the
 * scalar element), then called mg_pop_typed(mg, 1) - popping only ONE
 * tracked stackmap entry (the index), leaving the ARRAY's own reference
 * type ("[I") as the tracked top-of-stack type instead of the loaded
 * element's real type ("int"). This went unnoticed almost everywhere,
 * since a stale reference type is usually still assignable wherever it's
 * used - but a scalar int is never assignable from "[I", so the verifier
 * rejected it the moment a real branch merge (here, the ternary's own
 * frame, recorded after generating this array load in its else-branch)
 * had to reconcile this path against the other, correctly-typed one.
 * The identical bug existed in the compound-assignment ("arr[i] += x")
 * codegen's own current-value-load step, one function away, for the same
 * reason. Fixed by popping BOTH tracked operand slots (arrayref and
 * index) and pushing the load's actual result type, in both places.
 */
public class ArrayLoadStackmapMergeVerifyTest {
    static int[] buildPlan() {
        return new int[]{5, 6, 7};
    }

    static Object pick(int[] arr) {
        return arr.length == 0 ? null : new Object();
    }

    static int consume(int a, int b, int c) {
        return a + b + c;
    }

    static int run() {
        int[] plan = buildPlan();
        Object targetServer = plan.length == 0 ? null : pick(plan);
        int result = consume(1, plan.length == 0 ? 0 : plan[0], 3);
        return targetServer == null ? -1 : result;
    }

    public static void main(String[] args) {
        int result = run();
        if (result != 9) {
            throw new AssertionError("expected 9, got " + result);
        }
        System.out.println("ArrayLoadStackmapMergeVerifyTest passed!");
    }
}
