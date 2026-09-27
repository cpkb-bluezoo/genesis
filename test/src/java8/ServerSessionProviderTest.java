/** Subinterface inherits generic return type from ServerSessionProvider&lt;S&gt;. */
public class ServerSessionProviderTest {

    interface Listener {
    }

    interface Handler {
        void run();
    }

    interface ServerSessionProvider<S> {
        S openSession(Listener listener);
    }

    interface Provider extends ServerSessionProvider<Handler> {
    }

    static Handler open(Provider p, Listener l) {
        return p.openSession(l);
    }

    public static void main(String[] args) {
        System.out.println("ServerSessionProviderTest OK");
    }
}
