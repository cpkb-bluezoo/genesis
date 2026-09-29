package genericboundedtypevar;

/**
 * Calls Sink.token() from a DIFFERENT file than Sink itself, so the
 * call site's invokeinterface descriptor is built from the shared
 * registry's stub for Sink, not from Sink's own AST.
 */
public class User {
    static boolean go(Sink<Tok> s, Tok t) {
        return s.token(t, 1);
    }
}
