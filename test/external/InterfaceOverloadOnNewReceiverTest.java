import ifaceoverloadlib.Api;

/* Regression test: "new Impl().ask(x)" where the method is an overloaded
 * default method of an interface (itself loaded from a class file) the class
 * implements. The lookup through the class's interfaces matched on the name
 * alone and returned whichever overload it met first, so the call could be
 * emitted for a different arity (a VerifyError: the receiver where a String was
 * expected). Mirrors gumdrop's RealmDefaultsTest.scramUnsupportedByDefault. */
public class InterfaceOverloadOnNewReceiverTest {
    private static final class Impl implements Api { }

    public static void main(String[] args) {
        if (!"api1".equals(new Impl().ask("u"))) {
            throw new RuntimeException("1");
        }
        if (!"api2".equals(new Impl().ask("u", null))) {
            throw new RuntimeException("2");
        }
        if (!"api3".equals(new Impl().ask("u", null, 7))) {
            throw new RuntimeException("3");
        }
        if (!"apiII".equals(new Impl().ask(1, 2))) {
            throw new RuntimeException("II");
        }
        System.out.println("InterfaceOverloadOnNewReceiverTest passed!");
    }
}
