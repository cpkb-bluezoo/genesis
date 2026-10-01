/*
 * Bug: an anonymous class nested inside ANOTHER anonymous class, whose own
 * method contains a BRANCH (if) that reads a non-immediate enclosing
 * instance via an explicit qualified "Outer.this" (two lexical levels up),
 * produced a malformed classfile: "ClassFormatError: StackMapTable format
 * error: bad type array size". A single level of nesting with the exact
 * same branching pattern compiled and ran correctly - only the multi-hop
 * ("Outer.this" reached through TWO this$0 hops, not one) case was wrong.
 *
 * Root cause: codegen_load_enclosing_this()'s general multi-hop walk
 * (codegen_expr.c) used the plain, type-UNAWARE mg_pop(mg, 1) right
 * before each hop's type-aware mg_push_object(...) call. mg_pop() only
 * adjusts the abstract stack-depth counter (used for max_stack); it does
 * NOT remove the corresponding entry from mg->stackmap's own operand-
 * stack type-tracking array, which only mg_pop_typed()/the other "_typed"
 * push helpers touch. Net effect: mg->stack_depth stayed net-0 correctly
 * per hop (matching the real bytecode, which just replaces the top stack
 * slot via GETFIELD), but mg->stackmap's type array grew by ONE STALE,
 * leftover entry per hop - two hops (reaching a GRANDPARENT enclosing
 * class) left two bogus entries sitting underneath the real stack
 * contents, corrupting any StackMapTable frame recorded at a later
 * branch target inside the same expression (e.g. the implicit frame a
 * negated boolean expression needs for its own "jump around iconst_0/
 * iconst_1" pattern). The single-hop fast path (immediate enclosing class
 * only, a few lines above this general loop) already used the correct
 * mg_pop_typed(mg, 1) - only the general, multi-hop loop had the bug,
 * which is why a single level of nesting was unaffected. Fixed by using
 * mg_pop_typed(mg, 1) in the general loop too, matching the fast path.
 */
public class DoublyNestedAnonymousBranchStackmapVerifyTest {
    String label = "outer";

    Runnable get() {
        return new Runnable() {
            public void run() {
                Runnable inner = new Runnable() {
                    public void run() {
                        if (!"outer".equals(DoublyNestedAnonymousBranchStackmapVerifyTest.this.label)) {
                            throw new RuntimeException("wrong label");
                        }
                    }
                };
                inner.run();
            }
        };
    }

    public static void main(String[] args) {
        new DoublyNestedAnonymousBranchStackmapVerifyTest().get().run();
        System.out.println("DoublyNestedAnonymousBranchStackmapVerifyTest passed!");
    }
}
