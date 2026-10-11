/*
 * Regression test: an anonymous class created with arguments for a varargs superclass
 * constructor passed them to its own constructor, and on to super(...), one by one rather
 * than as the array the superclass constructor takes (VerifyError). The creation packs the
 * arguments of the varargs position into the array.
 */
public class AnonymousVarargsSuperConstructorTest {

    static class Base {
        final String seen;

        Base(String first, Class<?>... rest) {
            this.seen = first + rest.length;
        }
    }

    static class Plain {
        final String seen;

        Plain(String... rest) {
            this.seen = "p" + rest.length;
        }
    }

    static class Ints {
        final String seen;

        Ints(int first, int... rest) {
            int sum = first;
            for (int r : rest) {
                sum += r;
            }
            this.seen = "i" + sum;
        }
    }

    static class Generic<E> {
        final String seen;

        Generic(E... items) {
            this.seen = "g" + items.length + items[0];
        }
    }

    static class Bounded<T extends Number> {
        final String seen;

        Bounded(String name, T... items) {
            this.seen = name + items.length;
        }
    }

    @SuppressWarnings("unchecked")
    public static void main(String[] args) {
        Base base = new Base("x", String.class, Integer.class) { };
        Base none = new Base("y") { };
        Plain plain = new Plain("p", "q") { };
        Plain array = new Plain(new String[] {"a"}) { };
        Ints ints = new Ints(1, 2, 3) { };
        Ints single = new Ints(5) { };
        Generic<String> generic = new Generic<String>("a", "b") { };
        Bounded<Integer> bounded = new Bounded<Integer>("m", 1, 2, 3) { };
        String actual = base.seen + none.seen + plain.seen + array.seen + ints.seen + single.seen
            + generic.seen + bounded.seen;
        if (!"x2y0p2p1i6i5g2am3".equals(actual)) {
            throw new RuntimeException("FAIL: " + actual);
        }
        System.out.println("PASS");
    }
}
