import protectedclasslib.Base;

/*
 * Bug: a "protected" nested class's own top-level classfile access_flags
 * had ACC_PROTECTED (0x0004) set directly on it. JVMS Table 4.1-A defines
 * no such bit for a class file's own access_flags - only ACC_PUBLIC (or
 * neither, for package-private) is valid there; a nested type's true
 * "protected" visibility belongs solely in the ENCLOSING class's
 * InnerClasses attribute (informational, read by reflection), which
 * genesis already emitted correctly. The malformed access_flags caused
 * the JVM's own class-access check to reject the class from any other
 * package - even from a legitimate subclass, which JLS 6.6.2 permits -
 * with "IllegalAccessError: failed to access class ...". Confirmed
 * against gumdrop's own WebSocketClientProtocolHandler (in package
 * org.bluezoo.gumdrop.websocket.client) extending
 * HttpClientProtocolHandler (in org.bluezoo.gumdrop.http.client) and
 * referring to its inherited "protected enum ParseState" by simple name.
 * Fixed by promoting a protected nested class's own access_flags to
 * ACC_PUBLIC (matching real javac's own behavior, confirmed by disassembly)
 * so it stays loadable/accessible outside the package, and by dropping
 * ACC_PRIVATE to package-private (0) - the "protected"/"private"
 * restriction itself is enforced by the compiler at compile time, not by
 * the classfile's own access_flags, since the JVM has no such concept for
 * types.
 */
public class ProtectedNestedClassAccessTest extends Base {
    public boolean isRunning() {
        setRunning();
        return state == State.RUNNING;
    }

    public static void main(String[] args) {
        if (!new ProtectedNestedClassAccessTest().isRunning()) {
            throw new RuntimeException("expected isRunning() to be true");
        }
        System.out.println("ProtectedNestedClassAccessTest passed!");
    }
}
