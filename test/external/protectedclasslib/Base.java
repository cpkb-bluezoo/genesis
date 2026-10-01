package protectedclasslib;

/*
 * Deliberately compiled by genesis itself into a separate classfile (see
 * run-tests.sh) - mirrors gumdrop's own
 * HttpClientProtocolHandler.ParseState: a "protected" NESTED enum whose
 * only legal access from outside the declaring package is via a subclass
 * (JLS 6.6.2), exactly like WebSocketClientProtocolHandler extends
 * HttpClientProtocolHandler and refers to the inherited ParseState by
 * simple name.
 */
public class Base {
    protected enum State { IDLE, RUNNING }

    protected State state = State.IDLE;

    protected void setRunning() {
        state = State.RUNNING;
    }
}
