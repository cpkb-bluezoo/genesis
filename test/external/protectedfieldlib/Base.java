package protectedfieldlib;

/*
 * Deliberately compiled by genesis itself into a separate classfile (see
 * run-tests.sh) - mirrors gumdrop's own HttpClientProtocolHandler: a
 * "protected" instance FIELD, read from a subclass's own NESTED class (not
 * the subclass body directly), exactly like
 * WebSocketClientProtocolHandler$ClientWebSocketTransport reading the
 * inherited "protected Endpoint endpoint" field declared here.
 */
public class Base {
    protected int endpoint = 7;
}
