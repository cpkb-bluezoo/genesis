/*
 * Calling a method through a receiver whose type is a bounded type variable
 * (N extends Number) must dispatch on the bound.
 */
public class BoundedTypeVarCallTest {

    static class Bounded<N extends Number> {
        N n;

        Bounded(N n) {
            this.n = n;
        }

        int twice() {
            return n.intValue() * 2;
        }

        double half() {
            return n.doubleValue() / 2;
        }
    }

    static <T extends CharSequence> int len(T t) {
        return t.length();
    }

    public static void main(String[] args) {
        Bounded<Integer> b = new Bounded<Integer>(Integer.valueOf(21));
        if (b.twice() != 42) {
            System.out.println("FAILED: twice() = " + b.twice());
            System.exit(1);
        }
        if (b.half() != 10.5) {
            System.out.println("FAILED: half() = " + b.half());
            System.exit(1);
        }
        if (len("hello") != 5) {
            System.out.println("FAILED: len()");
            System.exit(1);
        }
        System.out.println("BoundedTypeVarCallTest passed!");
    }
}
