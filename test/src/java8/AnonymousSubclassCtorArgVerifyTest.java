/* Regression test: an anonymous subclass of a (source-declared) class whose
 * constructor takes a SUPERTYPE of the argument actually passed - here
 * "new Store(this) { ... }" inside Factory, where Store(Base) and Factory
 * extends Base. Two descriptors disagreed: the call site typed the argument
 * "this" as Object (NoSuchMethodError on the anonymous class's constructor),
 * and the anonymous class's own super(...) call was built from the
 * argument's static type (Factory) instead of the superclass constructor's
 * declared parameter (Base): no such constructor. Mirrors gumdrop's
 * POP3AuthFlowsTest.FailingFactory: "new StubMailboxStore(this) { ... }". */
public class AnonymousSubclassCtorArgVerifyTest {
    static class Base {
    }

    static class Store {
        final Base owner;

        Store(Base owner) {
            this.owner = owner;
        }

        String name() {
            return "store";
        }
    }

    static class Factory extends Base {
        Store create() {
            return new Store(this) {
                @Override
                String name() {
                    return "anon";
                }
            };
        }
    }

    public static void main(String[] args) {
        Factory f = new Factory();
        Store s = f.create();
        if (!"anon".equals(s.name()) || s.owner != f) {
            throw new RuntimeException("anonymous subclass: " + s.name());
        }
        System.out.println("AnonymousSubclassCtorArgVerifyTest passed!");
    }
}
