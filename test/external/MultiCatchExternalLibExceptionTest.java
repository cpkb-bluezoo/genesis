/*
 * A multi-catch pairing a SOURCE-COMPILED exception type
 * (MultiCatchLocalException, a SEPARATE top-level file compiled in the
 * same genesis invocation as this one - see its own comment for why that
 * matters) with one loaded only from a class file
 * (lib.ExternalLibException, compiled separately and referenced here via
 * -cp, no -sourcepath - see run-tests.sh) must still compute the right
 * least-upper-bound parameter type.
 *
 * find_common_ancestor_symbol() walked both exception types' ancestor
 * chains correctly, but compared candidates by POINTER identity. Each
 * file compiled in a batch gets its own semantic context, so the
 * classfile-loaded side's ancestor chain (symbol_from_classfile_minimal())
 * and the OTHER file's source-compiled side can each resolve
 * java.lang.Exception/Throwable/Object to their OWN separately-allocated
 * symbol_t - different instances of the "same" class - so even the one
 * ancestor every class shares, Object, never compared equal. The search
 * always fell through to "no common ancestor", silently defaulting the
 * multi-catch parameter's type to something too wide (Object, or
 * Throwable, depending on which side's chain happened to run out of
 * ancestors to compare first) for handleFailure(Exception), whose only
 * real overload takes Exception - so overload resolution fabricated a
 * nonexistent descriptor instead: NoSuchMethodError, or (as reproduced
 * here) a VerifyError from a checkcast/invoke built against the wrong
 * type. Confirmed against gumdrop's own GrpcClient, whose
 * "catch (ProtoParseException | ProtobufParseException e)" pairs a
 * source-compiled type (a sibling file in the same module) with one
 * loaded from lib/jprotobuf-1.0.0.jar - exactly this shape. (An earlier
 * draft of this test declared the local exception as a NESTED class in
 * this same file, and a JDK exception - java.text.ParseException - as
 * the classfile-loaded side; neither reliably reproduced the bug, since
 * both end up resolved within/cached by the same single-file semantic
 * pass rather than two independently-resolved batch members.)
 */
import lib.ExternalLibException;

public class MultiCatchExternalLibExceptionTest {
    static String lastMessage;

    static void handleFailure(Exception e) {
        lastMessage = e.getMessage();
    }

    static void thrower(boolean useExternal) throws MultiCatchLocalException, ExternalLibException {
        if (useExternal) {
            throw new ExternalLibException("external-side");
        } else {
            throw new MultiCatchLocalException("local-side");
        }
    }

    static void handle(boolean useExternal) {
        try {
            thrower(useExternal);
        } catch (MultiCatchLocalException | ExternalLibException e) {
            handleFailure(e);
        }
    }

    public static void main(String[] args) {
        handle(false);
        if (!"local-side".equals(lastMessage)) {
            System.out.println("FAILED: expected local-side, got " + lastMessage);
            System.exit(1);
        }
        handle(true);
        if (!"external-side".equals(lastMessage)) {
            System.out.println("FAILED: expected external-side, got " + lastMessage);
            System.exit(1);
        }
        System.out.println("MultiCatchExternalLibExceptionTest passed!");
    }
}
