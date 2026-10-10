/* Regression test: a local declared without an initializer before a switch
 * and assigned in only one branch ("default"), which also uses try/finally.
 * The stack-map frame at the end of the switch claimed the local was alive
 * (typed as assigned) although the other branch reaches that point with it
 * unset, so the verifier rejected the method: "Type top (current frame,
 * locals[6]) is not assignable to 'java/nio/ByteBuffer' (stack map)".
 * Mirrors gumdrop's HttpProtocolHandler.sendResponseHeaders. */
public class SwitchUnassignedLocalJoinVerifyTest {
    enum State { HTTP1, HTTP2 }

    State state = State.HTTP1;
    StringBuilder out = new StringBuilder();

    void send(StringBuilder b) {
        out.append(b);
    }

    void write(int streamId, String name) {
        out.append("h2:").append(streamId).append(name);
    }

    void respond(int streamId, boolean endStream) {
        StringBuilder buf;
        boolean success = false;
        switch (state) {
            case HTTP2:
                write(streamId, "x");
                break;
            default:
                buf = new StringBuilder("h1:");
                while (!success) {
                    try {
                        buf.append(streamId);
                        success = true;
                    } catch (RuntimeException e) {
                        buf = new StringBuilder("retry");
                    }
                }
                try {
                    send(buf);
                } finally {
                    buf.setLength(0);
                }
        }
        if (endStream) {
            out.append(";end");
        }
    }

    public static void main(String[] args) {
        SwitchUnassignedLocalJoinVerifyTest t = new SwitchUnassignedLocalJoinVerifyTest();
        t.respond(3, true);
        t.state = State.HTTP2;
        t.respond(5, false);
        if (!"h1:3;endh2:5x".equals(t.out.toString())) {
            throw new RuntimeException(t.out.toString());
        }
        System.out.println("SwitchUnassignedLocalJoinVerifyTest passed!");
    }
}
