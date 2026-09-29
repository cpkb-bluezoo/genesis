package genericboundedtypevar;

/**
 * Bug: a cross-file call to a method whose parameter is a class-level
 * BOUNDED type variable (Sink<T extends Enum<T>>.token(T, int)) erased
 * to Object instead of Enum at the invokeinterface call site, because
 * create_type_stub() (src/genesis.c) - which builds the shared registry
 * stub other files in the same compile batch resolve Sink's type
 * parameter through - always left the type variable's bound NULL.
 * Sink.class itself (compiled from its own AST) got the right
 * "(Ljava/lang/Enum;I)Z" descriptor; User.go(), compiled from a
 * different file in the same batch, called it with the wrong
 * "(Ljava/lang/Object;I)Z" - a NoSuchMethodError at runtime, since
 * interface method resolution requires an exact descriptor match.
 */
public class GenericBoundedTypeVarErasureVerifyTest implements Sink<Tok> {
    public boolean token(Tok type, int n) {
        return type == Tok.A && n == 1;
    }

    public static void main(String[] args) {
        GenericBoundedTypeVarErasureVerifyTest impl = new GenericBoundedTypeVarErasureVerifyTest();
        if (!User.go(impl, Tok.A)) {
            throw new RuntimeException("Sink.token() call via User.go() failed or returned wrong result");
        }
        if (User.go(impl, Tok.B)) {
            throw new RuntimeException("Sink.token() call via User.go() should have returned false for Tok.B");
        }
    }
}
