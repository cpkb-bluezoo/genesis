/*
 * Regression test: a subclass's explicit "super(...)" call to a *bounded*
 * generic superclass constructor (e.g. "BoundedGenericBase(T value)"
 * inside "class BoundedGenericBase<T extends Enum<T>>"), where the
 * superclass is loaded from its already-compiled .class file (via -cp,
 * not -sourcepath - exactly as happens whenever a multi-module build
 * forks the compiler once per module) - used to throw at the first
 * construction:
 *
 *   java.lang.NoSuchMethodError: 'void BoundedGenericBase.<init>
 *   (java.lang.Object)'
 *
 * The earlier (unbounded) generic-superclass-constructor fix
 * (GenericSuperCtorErasureTest) made codegen build the invokespecial
 * descriptor from the target constructor's own resolved parameter types
 * instead of the call's arguments - but a *bounded* type variable like
 * "T extends Enum<T>" erases to its bound (Enum), not plain Object, and
 * two separate, independent spots in semantic.c's classfile-loading code
 * discarded that bound entirely when reconstructing a type variable from
 * a classfile's generic Signature attribute:
 *
 * 1. When loading a *class's own* type parameters (e.g. registering
 *    "<T extends Enum<T>>" on BoundedGenericBase itself), the parsed
 *    class_bound was ignored and the bound hardcoded to NULL.
 * 2. A constructor or method parameter/return type that's merely a
 *    *reference* to that class-level type variable (e.g. "T value") only
 *    ever carries the variable's bare name in its own signature entry -
 *    the bound lives solely at the type parameter's declaration site
 *    (fixed by #1) - but nothing cross-referenced that declaration by
 *    name to recover the bound for the reference.
 *
 * Both erasing to the JLS-default (unbounded -> Object) meant the same
 * mismatched-descriptor problem as the unbounded case, just for the
 * bound type instead. Fixed by (1) actually converting the parsed
 * class_bound via generic_type_to_type() instead of discarding it, and
 * (2) a new patch_typevar_bound_from_class() that looks up a bare type-
 * variable reference's name against the enclosing class's own (now
 * correctly bounded) type parameters and copies the bound over, applied
 * to both a loaded method's return type and each of its parameters.
 */
import lib.BoundedGenericBase;

public class BoundedTypeVarErasureTest {
    enum Tok { A, B }

    static class SubLexer extends BoundedGenericBase<Tok> {
        SubLexer() {
            super(Tok.A);
        }
    }

    public static void main(String[] args) {
        /* Constructing this - which requires a correct invokespecial
         * descriptor for the super(...) call, erased to the bound
         * (Enum), not plain Object - is what this test exercises; a
         * NoSuchMethodError here is the bug. */
        new SubLexer();
        System.out.println("BoundedTypeVarErasureTest passed!");
    }
}
