package nestedimplinherited.p;

/* Regression test: a nested class whose "implements" clause names a member
 * type that the ENCLOSING class inherits from its own interface
 * (JLS 8.5): MockListener implements Listener, where Listener is declared
 * in Transport and MockTransport implements Transport. genesis left the
 * nested class's interface unresolved, so every @Override on its methods
 * was rejected ("does not override a method from its superclass or
 * interfaces") and returning a MockListener as a Listener failed
 * ("Incompatible return type"). Mirrors gumdrop's MockFtpDataTransport /
 * MockSocksTransport. */
final class NestedImplementsInheritedMemberTypeVerifyTest implements Transport {
    static final class MockListener implements Listener {
        final int port;
        boolean closed;

        MockListener(int port) {
            this.port = port;
        }

        @Override
        public int port() {
            return port;
        }

        @Override
        public void close() {
            closed = true;
        }
    }

    @Override
    public Listener listen(int port) {
        MockListener l = new MockListener(port);
        return l;
    }

    public static void main(String[] args) {
        Listener l = new NestedImplementsInheritedMemberTypeVerifyTest().listen(7);
        if (l.port() != 7) {
            throw new RuntimeException("port " + l.port());
        }
        l.close();
        System.out.println("PASS");
    }
}
