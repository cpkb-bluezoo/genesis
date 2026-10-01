import protectedmethodreceiverlib.Base;
import protectedmethodreceiverlib.Endpoint;
import java.nio.ByteBuffer;

/*
 * Bug: a method called on a receiver that is itself an inherited
 * "protected" field, reached through an ENCLOSING class's superclass
 * chain from a nested class (the exact same shape fix #18 made
 * accessible), resolved to the WRONG method descriptor -
 * "NoSuchMethodError: 'int Endpoint.send(ByteBuffer)'" at runtime, even
 * though Endpoint.send(ByteBuffer) returns void. Confirmed against
 * gumdrop's own WebSocketClientProtocolHandler$ClientWebSocketTransport.
 * sendFrame(), which calls "endpoint.send(frameData)" where "endpoint" is
 * the inherited "protected Endpoint endpoint" field from
 * HttpClientProtocolHandler.
 *
 * Root cause: semantic.c's AST_METHOD_CALL receiver resolution, when the
 * receiver is a bare identifier, checked whether it named a field
 * inherited from the CURRENT class's own superclass chain - but never
 * checked an ENCLOSING class's superclass chain (the codegen-side
 * equivalent lookup, fixed for #18, already did this correctly). Since
 * "endpoint" is inherited into the ENCLOSING class (the outer subclass),
 * not the nested class doing the calling, this lookup found nothing, so
 * the receiver's type was never resolved and the method call fell back
 * to guessing an "int" return type from the argument list alone (see
 * build_method_descriptor()'s own "Default to int" fallback comment).
 * Fixed by extending the same lookup to also walk each enclosing class's
 * own superclass chain, exactly mirroring codegen_expr.c's already-correct
 * equivalent.
 */
public class ProtectedMethodReceiverViaNestedClassTest extends Base {
    class Inner {
        void doSend(ByteBuffer data) {
            endpoint.send(data);
        }
    }

    public static void main(String[] args) {
        ProtectedMethodReceiverViaNestedClassTest t = new ProtectedMethodReceiverViaNestedClassTest();
        final int[] received = new int[1];
        t.endpoint = new Endpoint() {
            public void send(ByteBuffer data) {
                received[0] = data.remaining();
            }
        };
        t.new Inner().doSend(ByteBuffer.allocate(5));
        if (received[0] != 5) {
            throw new RuntimeException("expected 5 but got " + received[0]);
        }
        System.out.println("ProtectedMethodReceiverViaNestedClassTest passed!");
    }
}
