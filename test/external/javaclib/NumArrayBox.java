/* Library for TypeVarArrayFieldTest (built by javac, which writes the field
 * Signature attributes genesis does not): members typed as arrays of a bounded
 * type variable (N[], N[][]), read back from the class file. */
public class NumArrayBox<N extends Number> {
    public N[] arr;
    public N[][] grid;

    public NumArrayBox(N[] a, N[][] g) {
        arr = a;
        grid = g;
    }

    public N[] get() {
        return arr;
    }

    public double sum(N[] xs) {
        double s = 0;
        for (N n : xs) {
            s += n.doubleValue();
        }
        return s;
    }
}
