/*
 * Regression test: a circular type dependency between two constructors
 * across two separate files - A's constructor takes a B, B's constructor
 * takes an A (here: this test class constructs a CircularCtorHelper,
 * whose own constructor takes an instance of this class) - used to
 * produce a class file that threw at runtime:
 *
 *   java.lang.NoSuchMethodError: 'void CircularCtorHelper.<init>(java.lang.Object)'
 *
 * even though compilation itself reported no errors.
 *
 * In genesis's parallel batch compile, when this file's constructor call
 * `new CircularCtorHelper(this)` is resolved, CircularCtorHelper's class
 * symbol comes from the shared cross-file registry - a stub, since
 * CircularCtorHelper.java may still be mid-compile in another thread. That
 * stub gets "completed" (its members populated from its own AST) via
 * enter_members_for_type(), lazily, the first time it's needed. But that
 * function only ever registered AST_METHOD_DECL members - never
 * AST_CONSTRUCTOR_DECL - so a lazily-completed stub's constructors were
 * always missing entirely. With no constructor symbol to resolve against,
 * codegen for `new CircularCtorHelper(this)` fell back to inferring the
 * invokespecial's descriptor from the *argument* expression instead of the
 * real declared parameter type, producing a call site descriptor that
 * never matched the constructor CircularCtorHelper.java itself compiled
 * (which resolves its own parameter type correctly, since that never goes
 * through the lazy-completion path for its *own* file).
 *
 * A second, related gap surfaced once constructors were registered at all:
 * enter_members_for_type()'s parameter extraction (shared with methods)
 * only stored each parameter's *unresolved* type, leaving param_sym->type
 * NULL - fine for a class compiled normally (some later phase resolves it
 * from context), but a lazily-completed stub has no such later phase
 * coming, so a constructor "found" this way still had every parameter's
 * type as NULL, and the invokespecial descriptor came out as an empty
 * "()V" instead of the real one.
 *
 * This bug needs two source files compiled *together* in one batch to
 * reproduce - a single file (or two files compiled in separate genesis
 * invocations) doesn't exercise the shared-registry stub/lazy-completion
 * path at all. See genesis history for details (search "enter_members" and
 * "is_ctor" in semantic.c).
 */
public class CircularCtorDependencyTest {
    final CircularCtorHelper helper;

    private CircularCtorDependencyTest() {
        this.helper = new CircularCtorHelper(this);
    }

    public static void main(String[] args) {
        CircularCtorDependencyTest t = new CircularCtorDependencyTest();
        if (t.helper.owner != t) {
            throw new RuntimeException("owner mismatch");
        }
        System.out.println("All tests passed!");
    }
}
