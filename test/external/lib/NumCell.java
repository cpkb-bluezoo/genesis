package lib;
public class NumCell<N extends Number> {
    public N n;
    public NumCell(N n) { this.n = n; }
    public void set(N n) { this.n = n; }
    public N get() { return n; }
}
