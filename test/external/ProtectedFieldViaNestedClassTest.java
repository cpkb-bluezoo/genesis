import protectedfieldlib.Base;

/*
 * Bug: a "protected" field inherited from a superclass in a DIFFERENT
 * runtime package, read from a NESTED class of the subclass (not the
 * subclass body itself), threw "IllegalAccessError: tried to access
 * protected field ..." at runtime. JVMS 5.4.4's protected-access check
 * requires the class containing the getfield instruction to itself be a
 * subclass of the field's declaring class when the two are in different
 * packages - Sub qualifies, but Sub$Inner does not (it's a nested class OF
 * the subclass, not itself a subclass), so a direct getfield from Inner is
 * illegal even though the access is perfectly legal Java (JLS 6.6.2: a
 * protected member is accessible anywhere in the subclass's own body,
 * which includes its nested classes). Confirmed against gumdrop's own
 * WebSocketClientProtocolHandler$ClientWebSocketTransport (a nested class
 * of WebSocketClientProtocolHandler, package org.bluezoo.gumdrop.websocket.
 * client) reading the inherited "protected Endpoint endpoint" field
 * declared on HttpClientProtocolHandler (org.bluezoo.gumdrop.http.client).
 * Fixed by routing the read through a synthetic static accessor method
 * generated on the SUBCLASS (Sub) - exactly like real javac's own
 * access$NNN bridge methods - since the subclass itself, unlike its nested
 * class, does satisfy the JVMS check and can read the field directly.
 */
public class ProtectedFieldViaNestedClassTest extends Base {
    class Inner {
        int get() {
            return endpoint;
        }
    }

    Inner makeInner() {
        return new Inner();
    }

    public static void main(String[] args) {
        int v = new ProtectedFieldViaNestedClassTest().makeInner().get();
        if (v != 7) {
            throw new RuntimeException("expected 7 but got " + v);
        }
        System.out.println("ProtectedFieldViaNestedClassTest passed!");
    }
}
