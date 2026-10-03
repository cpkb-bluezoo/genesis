import indirectbridgelib.Conn;
import indirectbridgelib.Provider;

/* Regression test: a class implementing an interface that extends a GENERIC
 * interface loaded from a CLASS FILE ("interface Specific extends
 * Provider<Conn>", Provider compiled earlier) must get the erased bridge
 * "Object open(String)" for the parent's method. genesis generated it only
 * when the generic parent came from the same compilation, so a call through
 * Provider<Conn> failed with AbstractMethodError. Mirrors gumdrop's
 * FtpServerSessionProvider (ftp module) extending ServerSessionProvider
 * (core module). */
public class IndirectGenericBridgeFromClassfileTest {
    interface Specific extends Provider<Conn> {
        default void start() {
        }
    }

    static final class Impl implements Specific {
        @Override
        public Conn open(String name) {
            return new Conn("impl:" + name);
        }
    }

    public static void main(String[] args) {
        Provider<Conn> p = new Impl();
        if (!"impl:x".equals(p.open("x").name)) {
            throw new RuntimeException("impl");
        }
        Provider<Conn> q = new Specific() {
            @Override
            public Conn open(String n) {
                return new Conn("anon:" + n);
            }
        };
        if (!"anon:y".equals(q.open("y").name)) {
            throw new RuntimeException("anon");
        }
        System.out.println("IndirectGenericBridgeFromClassfileTest passed!");
    }
}
