/*
 * Regression test: a ternary (conditional) expression whose two branches
 * have different reference types - one a concrete object, the other a
 * bare `null` literal - used to produce a class file that failed JVM
 * bytecode verification:
 *
 *   java.lang.VerifyError: Inconsistent stackmap frames at branch target N
 *   Reason: Type '<concrete type>' (current frame, stack[k]) is not
 *   assignable to null (stack map, stack[k])
 *
 * codegen_expr.c's AST_CONDITIONAL_EXPR handling records the stack-map
 * frame at the join point after the ternary using whatever mg->stackmap
 * happens to be tracking once the else branch finishes generating - which,
 * for `directory ? new TreeMap<...>() : null`, is the else branch's own
 * `aconst_null` push (tracked as the bottom "null" verification type). But
 * the then branch's goto also reaches this exact merge point, carrying a
 * real TreeMap reference - and a concrete object is not assignable to the
 * declared "null" type (null is the *narrowest* possible type, not a
 * wildcard). This is the same "join point framed from whichever branch
 * ran last, not a real merge of both incoming edges" bug already fixed for
 * if/else, try/catch and synchronized statements - here for a ternary
 * expression specifically, where (unlike those) both branches are always
 * live, so there's no "one side always terminates" shortcut: the fix uses
 * the ternary's own JLS 15.25-computed overall type (already available as
 * expr->sem_type) to correct the merged frame directly.
 * See genesis history for details (search "mg->stackmap's tracked
 * stack-top type at this point" in codegen_expr.c).
 */
public class TernaryNullMergeVerifyTest {
    static class Node {
        final boolean directory;
        final java.util.TreeMap<String, Node> children;
        Node(boolean directory) {
            this.directory = directory;
            this.children = directory ? new java.util.TreeMap<String, Node>() : null;
        }
    }

    public static void main(String[] args) {
        Node dir = new Node(true);
        Node leaf = new Node(false);
        if (dir.children == null) {
            throw new RuntimeException("directory node should have a non-null children map");
        }
        if (leaf.children != null) {
            throw new RuntimeException("leaf node should have a null children map");
        }
        System.out.println("All tests passed!");
    }
}
