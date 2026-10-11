/*
 * Regression test: "this.actual" for a field of type variable declared by a generic superclass
 * ("Holder<ACTUAL>", used as Holder<Item<?>>) was not cast back from its erasure: the receiver
 * of the call on it was an Object (VerifyError). The bare name "actual" was cast, but
 * "this.actual" was not, as "this" carries no type of its own.
 */
public class InheritedGenericFieldThroughThisTest {

    static class Holder<ACTUAL> {
        protected ACTUAL actual;

        Holder(ACTUAL actual) {
            this.actual = actual;
        }
    }

    static class Item<T> {
        String name() {
            return "item";
        }
    }

    static class Mine extends Holder<Item<?>> {
        Mine(Item<?> item) {
            super(item);
        }

        String viaThis() {
            return this.actual.name();
        }

        String bare() {
            return actual.name();
        }
    }

    static class OfString extends Holder<String> {
        OfString(String value) {
            super(value);
        }

        int length() {
            return this.actual.length();
        }
    }

    public static void main(String[] args) {
        Mine mine = new Mine(new Item<String>());
        String actual = mine.viaThis() + mine.bare() + new OfString("abc").length();
        if (!"itemitem3".equals(actual)) {
            throw new RuntimeException("FAIL: " + actual);
        }
        System.out.println("PASS");
    }
}
