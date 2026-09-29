import java.util.EnumSet;
import java.util.Set;
import java.util.Collections;

/*
 * Regression test: calling a generic varargs method from an EXTERNAL
 * (classfile-loaded) class whose type variable is bounded (e.g.
 * "<E extends Enum<E>> EnumSet<E> of(E first, E... rest)") - matching
 * gumdrop's own BasicRealm, which builds its supported-SASL-mechanisms
 * set via:
 *
 *   EnumSet.of(SaslMechanism.PLAIN, SaslMechanism.LOGIN,
 *       SaslMechanism.CRAM_MD5, SaslMechanism.DIGEST_MD5,
 *       SaslMechanism.SCRAM_SHA_256, SaslMechanism.EXTERNAL)
 *
 * (6 arguments - only the varargs overload "of(E first, E... rest)"
 * matches that arity, unlike the 5 fixed-arity overloads of(E),
 * of(E,E), ..., of(E,E,E,E,E)) - used to throw at class-verification
 * time:
 *
 *   java.lang.VerifyError: Bad type on operand stack
 *   Reason: Type '[Ljava/lang/Object;' (current frame, stack[1]) is not
 *   assignable to '[Ljava/lang/Enum;'
 *
 * Root cause: semantic.c's patch_typevar_bound_from_class() - which
 * back-fills a type variable REFERENCE's bound (e.g. "E" in a parameter
 * type) from the enclosing class's own declared type parameter, since a
 * bare reference parsed from a generic signature carries only the
 * variable's name, not its bound - only ever handled the case where the
 * type variable was the parameter's ENTIRE type. A varargs parameter's
 * own generic signature ("E...") parses as TYPE_ARRAY wrapping a
 * TYPE_TYPEVAR element, not a bare TYPE_TYPEVAR - the array wrapper
 * failed this function's own "is this a type variable" check immediately,
 * so the type variable nested inside it never got its bound patched in,
 * leaving it looking unbounded. Downstream, building the synthetic
 * varargs array (codegen_expr.c's varargs_element_type() /
 * erase_typevar_for_array()) erases an apparently-unbounded type
 * variable to plain java.lang.Object - wrong for EnumSet.of's real
 * erased descriptor, which uses the type variable's true bound, Enum
 * (JVMS/JLS erasure rules), not Object.
 *
 * Fixed by making patch_typevar_bound_from_class() recurse into a
 * TYPE_ARRAY's element type (and a parameterized TYPE_CLASS's own type
 * arguments) instead of only ever handling a bare, unwrapped type
 * variable reference directly.
 */
public class GenericVarargsTypeVarArrayBoundVerifyTest {
    enum Mechanism { PLAIN, LOGIN, CRAM_MD5, DIGEST_MD5, SCRAM_SHA_256, EXTERNAL }

    public static void main(String[] args) {
        Set<Mechanism> mechanisms = Collections.unmodifiableSet(EnumSet.of(
                Mechanism.PLAIN,
                Mechanism.LOGIN,
                Mechanism.CRAM_MD5,
                Mechanism.DIGEST_MD5,
                Mechanism.SCRAM_SHA_256,
                Mechanism.EXTERNAL));

        if (mechanisms.size() != 6) {
            throw new RuntimeException("expected 6 mechanisms, got " + mechanisms.size());
        }
        if (!mechanisms.contains(Mechanism.PLAIN) || !mechanisms.contains(Mechanism.EXTERNAL)) {
            throw new RuntimeException("expected PLAIN and EXTERNAL to be present, got " + mechanisms);
        }

        System.out.println("GenericVarargsTypeVarArrayBoundVerifyTest passed!");
    }
}
