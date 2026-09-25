/*
 * Primitive arguments passed to type-variable parameters must be boxed, for
 * constructors and for methods.
 */
public class TypeVarBoxingTest {

    static class Box<T> {
        T value;

        Box(T value) {
            this.value = value;
        }

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
    }

    static <T> T identity(T t) {
        return t;
    }

    public static void main(String[] args) {
        Box<Integer> box = new Box<Integer>(21);
        if (box.get().intValue() != 21) {
            System.out.println("FAILED: constructor boxing");
            System.exit(1);
        }
        box.set(7);
        if (box.get().intValue() != 7) {
            System.out.println("FAILED: method boxing");
            System.exit(1);
        }
        Bounded<Integer> bounded = new Bounded<Integer>(5);
        if (bounded.n.intValue() != 5) {
            System.out.println("FAILED: bounded constructor boxing");
            System.exit(1);
        }
        Integer id = identity(3);
        if (id.intValue() != 3) {
            System.out.println("FAILED: generic method boxing");
            System.exit(1);
        }
        Box<Long> wide = new Box<Long>(5L);
        Box<Double> real = new Box<Double>(2.5);
        if (wide.get().longValue() != 5L || real.get().doubleValue() != 2.5) {
            System.out.println("FAILED: two-slot constructor boxing");
            System.exit(1);
        }
        System.out.println("TypeVarBoxingTest passed!");
    }
}
