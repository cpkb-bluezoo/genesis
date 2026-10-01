package protectedmethodreceiverlib;

import java.nio.ByteBuffer;

/*
 * Deliberately compiled by genesis itself into a separate classfile (see
 * run-tests.sh) - mirrors gumdrop's own Endpoint: an interface whose sole
 * abstract method returns void, invoked on a receiver that is itself an
 * inherited protected field reached through an enclosing class's
 * superclass chain (see ProtectedMethodReceiverViaNestedClassTest's own
 * comment).
 */
public interface Endpoint {
    void send(ByteBuffer data);
}
