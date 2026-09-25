/*
 * Nested generic classes: type parameters must be visible in member
 * signatures, and method references must work with a functional interface
 * that inherits its single abstract method.
 */
public class NestedGenericTest {

    static class Box<T> {
        T value;

        void set(T v) {
            this.value = v;
        }

        T get() {
            return value;
        }
    }

    static class Bounded<N extends Number> {
        N n;

        Bounded(N n) {
            this.n = n;
        }

        N get() {
            return n;
        }
    }

    interface Source<T> {
        T next();
    }

    interface StringSource extends Source<String> {
    }

    static String greeting() {
        return "hi";
    }

    public static void main(String[] args) {
        Box<String> box = new Box<String>();
        box.set("x");
        if (!"x".equals(box.get())) {
            System.out.println("FAILED: Box");
            System.exit(1);
        }
        Bounded<Integer> bounded = new Bounded<Integer>(Integer.valueOf(21));
        Integer got = bounded.get();
        if (got.intValue() != 21) {
            System.out.println("FAILED: Bounded");
            System.exit(1);
        }
        StringSource src = NestedGenericTest::greeting;
        if (!"hi".equals(src.next())) {
            System.out.println("FAILED: inherited SAM method reference");
            System.exit(1);
        }
        System.out.println("NestedGenericTest passed!");
    }
}
