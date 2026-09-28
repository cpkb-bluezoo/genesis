/*
 * Regression test: a static method with a varargs parameter forwarding
 * that same parameter (already an array) directly to another varargs
 * method - declared in a *different* file compiled in the same batch -
 * used to fail semantic analysis entirely:
 *
 *   error: Incompatible return type: expected <Type>, got <unknown>
 *   (or, for a plain assignment instead of return:)
 *   error: Incompatible types: cannot convert <unknown> to <Type>
 *
 * This is the exact shape of gumdrop's own
 * AsyncFile.open(...) -> BlockingAsyncFile.open(...) (a static interface
 * method delegating to a same-signature static method on another class,
 * both varargs).
 *
 * Root cause: a class's methods and constructors, when first needed via
 * genesis's cross-file shared-registry stub (a class still being compiled
 * concurrently in the same parallel batch), get their members registered
 * lazily via enter_members_for_type(). That function's parameter-type
 * resolution (added to fix a related constructor bug - see
 * "CircularCtorDependencyTest") resolved each parameter's *written* type
 * directly, but a varargs parameter's written type (`T` in `T... name`) is
 * its *element* type, not its true declared type `T[]` - every other
 * varargs-aware type resolution path in genesis wraps the element type in
 * an array for exactly this reason. Without the same wrap here, a varargs
 * parameter registered via this lazy path ended up typed as its bare
 * element type instead of an array, so `find_best_method_by_types()`'s
 * array-to-array (pass-the-array-through) argument match against it always
 * failed, leaving the whole method call - and anything depending on its
 * type, like a `return` or an assignment - resolved to <unknown>.
 * See genesis history for details (search "A varargs parameter's own
 * written type is its" in semantic.c).
 */
public class VarargsForwardCrossFileTest {
    static VarargsForwardCrossFileHelper open(String path, String... options) {
        return VarargsForwardCrossFileHelper.open(path, options);
    }

    public static void main(String[] args) {
        VarargsForwardCrossFileHelper h = open("f", "a", "b", "c");
        if (!"f".equals(h.path) || h.optionCount != 3) {
            throw new RuntimeException("forwarding failed: path=" + h.path + " count=" + h.optionCount);
        }
        System.out.println("All tests passed!");
    }
}
