/*
 * Regression test: a subclass's explicit "super(...)" call to a generic
 * superclass constructor whose parameters are typed with the class's own
 * type variable (e.g. "GenericSuperCtor(int n, T first, T second)"),
 * where the superclass is loaded from its already-compiled .class file
 * (via -cp, not -sourcepath - exactly as happens whenever a multi-module
 * build forks the compiler once per module rather than compiling
 * everything in one invocation) - used to throw at the first
 * construction despite compiling and verifying cleanly:
 *
 *   java.lang.NoSuchMethodError: 'void GenericSuperCtor.<init>(int,
 *   SomeEnum, SomeEnum)'
 *
 * codegen_expr.c's explicit-constructor-call codegen (for "super(...)"
 * or "this(...)") built the invokespecial's descriptor with
 * build_method_descriptor(expr->data.node.children, NULL) - straight
 * from the *argument expressions'* own types - rather than from the
 * *target constructor's* own declared parameter types. A parameter typed
 * as the class's own type variable T erases to Object in its real,
 * compiled descriptor, but an enum constant argument passed for that
 * parameter is not - so the descriptor built from arguments named the
 * argument's own concrete type instead of Object, producing a descriptor
 * that didn't match the constructor actually compiled for the
 * superclass. semantic.c's own AST_EXPLICIT_CTOR_CALL handling already
 * resolved the matching target constructor symbol (to bind lambda/method-
 * reference arguments to its parameter types) but never stored it
 * anywhere, so codegen had no way to reach it and had to guess from the
 * arguments instead.
 *
 * Fixed by having semantic.c store the resolved constructor on
 * expr->sem_symbol, and having codegen build the descriptor from it
 * (method_to_descriptor()) instead of from the call's own arguments,
 * falling back to the old argument-inferred descriptor only if no target
 * constructor was resolved.
 */
import lib.GenericSuperCtor;

public class GenericSuperCtorErasureTest {
    enum Tok { A, B }

    static class SubLexer extends GenericSuperCtor<Tok> {
        SubLexer() {
            super(5, Tok.A, Tok.B);
        }
    }

    public static void main(String[] args) {
        /* Constructing this - which requires a correct invokespecial
         * descriptor for the super(...) call - is what this test
         * actually exercises; a NoSuchMethodError here is the bug. */
        new SubLexer();
        System.out.println("GenericSuperCtorErasureTest passed!");
    }
}
