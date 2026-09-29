/*
 * Regression test: a ternary expression whose two branches are unrelated
 * final classes both implementing a common interface (neither a subtype
 * of the other) - exactly gumdrop's own TcpEndpoint.onHandshakeComplete()
 * shape:
 *
 *   info = tls13 ? new HandshakeSecurityInfo(...) : new Tls12SecurityInfo(...);
 *
 * used to throw:
 *
 *   java.lang.VerifyError: Instruction type does not match stack map
 *   Type 'Tls12SecurityInfo' (current frame, stack[0]) is not assignable
 *   to 'HandshakeSecurityInfo' (stack map, stack[0])
 *
 * Root cause: get_expression_type()'s AST_CONDITIONAL_EXPR case (JLS
 * 15.25) already handled "one branch's type is a subtype of the
 * other's" (using the wider one), but fell back to "use the then
 * branch's type" whenever NEITHER branch is a subtype of the other -
 * exactly this shape, where both branches are sibling classes each
 * independently implementing a shared interface. That too-narrow type
 * (the then branch's own concrete class) then corrupted the ternary's
 * own join-point stack-map frame (codegen_expr.c's AST_CONDITIONAL_EXPR
 * handling uses this computed type for that correction) whenever the
 * runtime path actually took the else branch, whose real type didn't
 * fit the declared (then-branch) frame. Fixed with a new
 * find_common_ancestor_symbol() - a simplified least-upper-bound that
 * walks each branch's own interfaces (checked before its superclass
 * chain, so a specific shared interface is found before falling all the
 * way back to java.lang.Object) and returns the first ancestor common
 * to both.
 */
public class TernarySiblingInterfaceTypesVerifyTest {
    interface Info {
        String describe();
    }

    static final class HandshakeInfo implements Info {
        public String describe() {
            return "handshake";
        }
    }

    static final class Tls12Info implements Info {
        public String describe() {
            return "tls12";
        }
    }

    static Info make(boolean tls13, boolean flag) {
        Info info;
        if (flag) {
            info = tls13 ? new HandshakeInfo() : new Tls12Info();
        } else {
            info = tls13 ? new HandshakeInfo() : new Tls12Info();
        }
        return info;
    }

    public static void main(String[] args) {
        if (!make(false, true).describe().equals("tls12")) {
            throw new AssertionError("expected tls12");
        }
        if (!make(true, false).describe().equals("handshake")) {
            throw new AssertionError("expected handshake");
        }
        System.out.println("TernarySiblingInterfaceTypesVerifyTest passed!");
    }
}
