/* Regression test: an anonymous class, created inside an inner class, whose
 * superclass is an inner class of the OUTER class ("new SearchHandler() {...}"
 * in Lookup, where SearchHandler is declared in Realm and Lookup is too). The
 * anonymous class needs its own this$0 (the enclosing Lookup), distinct from
 * the one its superclass holds (the Realm); it was left without one and its
 * code failed with NoSuchFieldError "does not have member field ... this$0".
 * Mirrors gumdrop's LdapRealm.CertificateLookup. */
public class AnonymousExtendingInnerOfOuterOuterTest {
    private String attribute = "cn";

    abstract class Handler {
        String tag() {
            return "h:" + attribute;
        }

        abstract void entry(String value);
    }

    class Op {
        String result;

        void succeed(String r) {
            result = r;
        }
    }

    final class Lookup extends Op {
        private String found;

        void run() {
            Handler h = new Handler() {
                @Override
                void entry(String value) {
                    found = value + "@" + attribute;
                    succeed(found + "/" + tag());
                }
            };
            h.entry("x");
        }
    }

    public static void main(String[] args) {
        AnonymousExtendingInnerOfOuterOuterTest realm = new AnonymousExtendingInnerOfOuterOuterTest();
        Lookup l = realm.new Lookup();
        l.run();
        if (!"x@cn/h:cn".equals(l.result)) {
            throw new RuntimeException(l.result);
        }
        if (!"x@cn".equals(l.found)) {
            throw new RuntimeException(l.found);
        }
        System.out.println("AnonymousExtendingInnerOfOuterOuterTest passed!");
    }
}
