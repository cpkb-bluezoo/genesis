/**
 * Bug: a multi-catch clause's exception parameter, "catch (A | B e)",
 * always got typed as the FIRST alternative (A), both in semantic.c's own
 * symbol for it and, independently, in codegen_stmt.c's local-variable
 * allocation for it (two separate, un-synced computations, one per file -
 * this codebase's usual pattern for logic implemented more than once).
 * Loading "e" later then emitted a checkcast against that first
 * alternative's type unconditionally. When the SECOND alternative (B) was
 * the one actually thrown at runtime, that checkcast failed:
 * ClassCastException, even though the code is perfectly legal Java and
 * "e" is only ever used in ways valid for both alternatives (JLS 14.20:
 * the parameter's real type is the least upper bound of every
 * alternative, not just the first). Confirmed against gumdrop's own
 * GrpcClient, whose "catch (ProtoParseException | ProtobufParseException e)"
 * is exactly this shape, and only surfaced because the SPECIFIC test case
 * that exercises the code path throws the second alternative, not the
 * first (which would have masked the bug completely, since a checkcast to
 * the exact runtime type it was thrown as always succeeds).
 */
public class MultiCatchSecondAlternativeThrownVerifyTest {
    static class ExcA extends Exception {
        ExcA(String m) { super(m); }
    }

    static class ExcB extends Exception {
        ExcB(String m) { super(m); }
    }

    static void thrower(boolean useB) throws ExcA, ExcB {
        if (useB) {
            throw new ExcB("from-b");
        } else {
            throw new ExcA("from-a");
        }
    }

    static String handle(boolean useB) {
        try {
            thrower(useB);
            return "no exception";
        } catch (ExcA | ExcB e) {
            return e.getMessage();
        }
    }

    public static void main(String[] args) {
        if (!"from-a".equals(handle(false))) {
            throw new RuntimeException("expected from-a, got " + handle(false));
        }
        if (!"from-b".equals(handle(true))) {
            throw new RuntimeException("expected from-b, got " + handle(true));
        }
    }
}
