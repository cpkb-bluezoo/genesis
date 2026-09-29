/*
 * Regression test: an enum constant's constructor argument list containing
 * a hex integer literal with a lowercase hex digit that happens to be the
 * letter 'e' (or 'E'), or the boolean literal `true`/`false` (which itself
 * contains the letter 'e') - exactly gumdrop's own NamedGroup enum shape:
 *
 *   X25519_MLKEM768(0x11ec, 1184 + 32, 1088 + 32, true),
 *
 * used to throw:
 *
 *   java.lang.VerifyError: Bad type on operand stack
 *   Type integer (current frame, stack[N]) is not assignable to double_2nd
 *
 * from the enum's own <clinit>.
 *
 * Root cause: codegen.c's enum-constant-instantiation codegen (used to
 * build each constant's own `new EnumType(...)` + invokespecial in
 * <clinit>) determines each constructor argument's type for the
 * invokespecial's descriptor from `arg->sem_type` when available, but -
 * for a bare literal argument, whose own codegen never needs to call
 * get_expression_type() and so never gets self-annotated - fell back to
 * a fragile heuristic: "does the literal's own source text contain '.',
 * 'e', or 'E'?" to detect a double/float literal. This misfired for any
 * hex integer literal with 'e'/'E' as one of its hex digits (unrelated
 * to scientific notation) and for the literal "true" itself (which
 * contains a trailing 'e') - both got misclassified as `double`,
 * corrupting the invokespecial's descriptor with spurious category-2
 * 'D' parameters the actual (all-int/boolean) pushed arguments never
 * matched. Fixed by asking the semantic analyzer directly
 * (get_expression_type()) for any argument whose sem_type isn't already
 * set, instead of guessing from the literal's source text.
 *
 * This shape needs a second, independent fix to even run at all: the
 * 3-arg constructor's own `this(code, clientLen, serverLen, false)` call
 * (delegating to the 4-arg constructor) used to throw
 * `NoSuchMethodError: void Group.<init>(int, int, int, boolean)` -
 * missing the enum's own compiler-added (String name, int ordinal)
 * leading parameters entirely, both from the pushed arguments (never
 * forwarding the CURRENT constructor's own slot 1/2) and from the
 * invokespecial's descriptor (built solely from the resolved target
 * constructor's user-declared parameters). Fixed in
 * codegen_explicit_ctor_call() (codegen_expr.c) by detecting a `this(...)`
 * call inside an enum's own constructor and forwarding aload_1/iload_2
 * plus prepending "Ljava/lang/String;I" to the descriptor, matching the
 * enum-constant-instantiation codegen's own equivalent prefix.
 */
public class EnumCtorHexLiteralArgVerifyTest {
    enum Group {
        A(0x001d, 32, 32),
        B(0x11ec, 1184 + 32, 1088 + 32, true),
        C(0x11eb, 65 + 1184, 65 + 1088, false);

        final int code;
        final int clientLen;
        final int serverLen;
        final boolean pqFirst;

        Group(int code, int clientLen, int serverLen) {
            this(code, clientLen, serverLen, false);
        }

        Group(int code, int clientLen, int serverLen, boolean pqFirst) {
            this.code = code;
            this.clientLen = clientLen;
            this.serverLen = serverLen;
            this.pqFirst = pqFirst;
        }
    }

    public static void main(String[] args) {
        if (Group.A.code != 0x001d || Group.A.clientLen != 32 || Group.A.pqFirst) {
            throw new AssertionError("A: unexpected field values");
        }
        if (Group.B.code != 0x11ec || Group.B.clientLen != 1216 || Group.B.serverLen != 1120 || !Group.B.pqFirst) {
            throw new AssertionError("B: unexpected field values");
        }
        if (Group.C.code != 0x11eb || Group.C.clientLen != 1249 || Group.C.serverLen != 1153 || Group.C.pqFirst) {
            throw new AssertionError("C: unexpected field values");
        }
        System.out.println("EnumCtorHexLiteralArgVerifyTest passed!");
    }
}
