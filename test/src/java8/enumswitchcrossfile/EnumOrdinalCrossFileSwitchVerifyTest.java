package enumswitchcrossfile;

/*
 * Regression test: a `switch` over an enum type declared in a SEPARATE
 * file, compiled together with that enum in a single genesis invocation
 * (the "same-batch, shared-type-registry" cross-file resolution path,
 * distinct from both same-file resolution and classfile-loading via
 * -cp) - exactly gumdrop's own DnsResolver.newTransportInstance() shape
 * - used to throw:
 *
 *   java.lang.VerifyError: Bad lookupswitch instruction
 *
 * with every case label's match key compiled as 0 instead of the
 * constant's real ordinal (0, 1, 2, 3, ...).
 *
 * Root cause: genesis.c's create_type_stub() - which builds a forward-
 * reference "stub" symbol_t for a type as soon as any file in the batch
 * is registered, so other files in the same batch can resolve it before
 * its own file's semantic analysis has run - populates each enum
 * constant's symbol_t with is_enum_constant = true but never set
 * data.var_data.enum_ordinal, leaving it at its calloc()'d default of 0
 * for every constant. The enum's own file's later, real pass1 processing
 * (semantic.c) does correctly pre-assign ordinals, but onto a SEPARATE,
 * disconnected symbol_t it creates for itself (top-level types don't get
 * decl->sem_symbol pre-linked to the registry stub the way nested types
 * do) - so the stub already sitting in the shared type registry, which
 * is what a cross-file switch statement's case-label resolution actually
 * looks up, never gets the correct ordinals. The switch-statement code
 * (also semantic.c) trusts is_enum_constant and reads enum_ordinal
 * without validating it, so it silently uses the always-zero value.
 * Fixed by having create_type_stub() assign each constant's ordinal by
 * declaration order, matching what the classfile/sourcepath-loading path
 * (load_class_from_source) already did correctly.
 */
public class EnumOrdinalCrossFileSwitchVerifyTest {
    static String name(TransportKind k) {
        switch (k) {
            case DOQ:
                return "doq";
            case DOT:
                return "dot";
            case DOH:
                return "doh";
            case PLAIN:
            default:
                return "plain";
        }
    }

    public static void main(String[] args) {
        String[] expected = { "doq", "dot", "doh", "plain" };
        TransportKind[] values = TransportKind.values();
        for (int i = 0; i < values.length; i++) {
            String got = name(values[i]);
            if (!got.equals(expected[i])) {
                throw new AssertionError(values[i] + " -> " + got + ", expected " + expected[i]);
            }
        }
        System.out.println("EnumOrdinalCrossFileSwitchVerifyTest passed!");
    }
}
