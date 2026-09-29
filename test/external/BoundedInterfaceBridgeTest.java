/*
 * Regression test: a class implementing a *bounded* generic interface
 * (e.g. "BoundedHandler<T extends Enum<T>>") with a concrete type
 * argument (e.g. "implements BoundedHandler<Tok>"), where the interface
 * is loaded from its already-compiled .class file (via -cp, not
 * -sourcepath - exactly as happens whenever a multi-module build forks
 * the compiler once per module) - used to throw at the first virtual
 * dispatch through the interface type:
 *
 *   java.lang.AbstractMethodError: Receiver class ...$Impl does not
 *   define or inherit an implementation of the resolved method
 *   'abstract boolean token(java.lang.Enum)' of interface
 *   lib.BoundedHandler.
 *
 * The earlier interface/abstract-method Signature-attribute fix
 * (NestedGenericInterfaceBridgeTest) and the bounded-type-variable
 * erasure fix (BoundedTypeVarErasureTest) together made genesis
 * correctly reconstruct, from a classfile's generic Signature
 * attribute, that BoundedHandler's own "T" is bound by Enum (not
 * Object) - but generate_interface_bridges() (and the analogous
 * generate_superclass_bridges()) in codegen.c still hardcoded
 * "Ljava/lang/Object;" for *every* TYPE_TYPEVAR parameter/return type
 * when building a bridge method's erased descriptor, instead of calling
 * the already-correct, bound-aware type_to_descriptor() (which erases a
 * TYPE_TYPEVAR to its bound, or Object only if unbounded). So the
 * concrete override's required bridge - token(Enum) -> token(Tok) - was
 * built with the wrong erased descriptor (token(Object)), which didn't
 * match BoundedHandler's *real* erased abstract method (token(Enum)),
 * so no bridge was found to satisfy it.
 *
 * Fixed by using type_to_descriptor() unconditionally in both bridge
 * generators instead of special-casing TYPE_TYPEVAR to Object. This in
 * turn exposed a second, latent bug: generate_superclass_bridges()
 * unconditionally synthesizes a super-forwarding "bridge" for any
 * inherited generic superclass method the subclass doesn't itself
 * override - which, for an *unbounded* type variable, redundantly (but
 * harmlessly) duplicated the inherited method's already-Object-erased
 * descriptor; but for a *bounded* one (e.g. java.lang.Enum's own final
 * "compareTo(T)", erased to compareTo(Enum) since Enum's own T is bound
 * by "T extends Enum<T>"), the "bridge" now exactly duplicated an
 * inherited *final* method's descriptor, which the JVM rejects
 * ("class ... overrides final method ... compareTo"). Fixed by skipping
 * that branch entirely for a final superclass method (which can never
 * legally be overridden/bridged in a subclass to begin with).
 */
import lib.BoundedHandler;

public class BoundedInterfaceBridgeTest {
    enum Tok { A, B }

    static class Impl implements BoundedHandler<Tok> {
        public boolean token(Tok type) {
            return type == Tok.A;
        }
    }

    public static void main(String[] args) {
        BoundedHandler<Tok> h = new Impl();
        /* Dispatch through the interface's own erased method - this is
         * what requires the bridge; an AbstractMethodError here is the
         * first bug, and an IncompatibleClassChangeError constructing
         * Tok itself (from the enum's own bogus superclass bridge) is
         * the second. */
        if (!h.token(Tok.A)) {
            throw new RuntimeException("token(Tok.A) should be true");
        }
        System.out.println("BoundedInterfaceBridgeTest passed!");
    }
}
