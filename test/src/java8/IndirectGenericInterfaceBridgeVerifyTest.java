/* Regression test: a class implementing an interface that itself extends a
 * GENERIC interface with a concrete type argument
 * ("interface Specific extends Provider<Conn>") must get a bridge method for
 * the generic parent's erased signature (Object open(String)). genesis only
 * bridged against interfaces the class lists directly, so a call through
 * Provider<Conn> failed with AbstractMethodError. Mirrors gumdrop's
 * FtpServerSessionProvider extends ServerSessionProvider<ClientConnected>. */
public class IndirectGenericInterfaceBridgeVerifyTest {
    interface Provider<S> {
        S open(String name);
    }

    static final class Conn {
        final String name;

        Conn(String name) {
            this.name = name;
        }
    }

    interface Specific extends Provider<Conn> {
        default void start() {
        }
    }

    interface Deeper extends Specific {
    }

    static final class Impl implements Specific {
        @Override
        public Conn open(String name) {
            return new Conn("impl:" + name);
        }
    }

    static final class DeepImpl implements Deeper {
        @Override
        public Conn open(String name) {
            return new Conn("deep:" + name);
        }
    }

    public static void main(String[] args) {
        Provider<Conn> p = new Impl();
        Conn c = p.open("a");
        if (!"impl:a".equals(c.name)) {
            throw new RuntimeException("impl: " + c.name);
        }
        Provider<Conn> d = new DeepImpl();
        if (!"deep:b".equals(d.open("b").name)) {
            throw new RuntimeException("deep");
        }
        System.out.println("IndirectGenericInterfaceBridgeVerifyTest passed!");
    }
}
