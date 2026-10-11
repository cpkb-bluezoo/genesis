package p;
/* "super(Entry::new)" in a subclass of a generic class compiled in the same batch: the method
 * reference is bound to BiFunction<String, Contributor, Entry>, the type arguments the extends clause gives. */
public class SuperConstructorMethodRefTest extends Registry<SuperConstructorMethodRefTest.Contributor, SuperConstructorMethodRefTest.Entry> {
    static class Contributor {
    }

    static class Entry {
        final String name;
        final Contributor contributor;

        Entry(String name, Contributor contributor) {
            this.name = name;
            this.contributor = contributor;
        }
    }

    SuperConstructorMethodRefTest() {
        super(Entry::new);
    }

    public static void main(String[] args) {
        Entry entry = new SuperConstructorMethodRefTest().make("x", new Contributor());
        if (!"x".equals(entry.name)) {
            throw new AssertionError(entry.name);
        }
        System.out.println("PASS");
    }
}
