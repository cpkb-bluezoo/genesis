import java.lang.reflect.Method;

/* Regression test: a varargs method with no body - an interface method,
 * an abstract class method, a native method - was written to its class
 * file WITHOUT the ACC_VARARGS access flag (only methods with a body got
 * it). Nothing goes wrong inside a single compile, where callers see the
 * source declaration. But a LATER, separate compile that loads the type
 * back from that class file (-cp; what every multi-module build does)
 * sees plain array parameters: no candidate accepts a variable number of
 * arguments, overload resolution finds nothing applicable, and the call
 * site falls back to a guess. With two overloads differing only in the
 * varargs element type, the guess was the wrong one:
 * "session.command(h, "CONFIG", "GET", "maxmemory")" was compiled as a
 * call to the byte[]... overload with the raw Strings pushed unwrapped,
 * failing verification ("Bad type on operand stack"). Mirrors gumdrop's
 * RedisSession.command(...) overloads, called from
 * RedisClientProtocolHandlerTest against build/redis's class files.
 *
 * The library (test/external/lib/Varargs*.java) is compiled first and
 * this file is then compiled against its class files only. */
public class ExternalVarargsFlagTest {

    private static void check(boolean ok, String what) {
        if (!ok) {
            throw new RuntimeException("failed: " + what);
        }
    }

    public static void main(String[] args) throws Exception {
        VarargsImpl impl = VarargsImpl.open();

        /* Interface methods. The byte[]... overload of command() is
         * declared but deliberately never CALLED here: it is what makes a
         * wrong guess observable, and calling it is a separate genesis
         * problem (choosing an overload whose varargs element type is
         * itself an array). */
        VarargsSession session = impl;
        String result = session.command("h", "CONFIG", "GET", "maxmemory");
        check("S:CONFIG:2".equals(result), "interface String... overload, got " + result);
        result = session.command("h", "GET", "key");
        check("S:GET:1".equals(result), "interface String... overload, one argument, got " + result);
        check(session.count(2L, 7, 8, 9) == 6, "interface primitive varargs");
        check(session.count(2L) == 0, "interface primitive varargs, no arguments");

        /* Abstract class methods */
        VarargsBase base = impl;
        result = base.join(3, "a", "b");
        check("J3:2".equals(result), "abstract String... method, got " + result);
        check(base.size("x", "y", impl) == 3, "abstract Object... method");
        check(base.size() == 0, "abstract Object... method, no arguments");

        /* The flag itself, on each kind of bodiless method */
        Method m = VarargsSession.class.getMethod("command", String.class, String.class, String[].class);
        check(m.isVarArgs(), "ACC_VARARGS on interface method (String...)");
        m = VarargsSession.class.getMethod("command", String.class, String.class, byte[][].class);
        check(m.isVarArgs(), "ACC_VARARGS on interface method (byte[]...)");
        m = VarargsBase.class.getMethod("join", int.class, String[].class);
        check(m.isVarArgs(), "ACC_VARARGS on abstract method (String...)");
        m = VarargsBase.class.getMethod("size", Object[].class);
        check(m.isVarArgs(), "ACC_VARARGS on abstract method (Object...)");
        m = VarargsImpl.class.getMethod("unused", int[].class);
        check(m.isVarArgs(), "ACC_VARARGS on native method");

        System.out.println("PASS");
    }
}
