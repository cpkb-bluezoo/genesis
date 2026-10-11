package p;
public non-sealed interface Iter extends Source {
    @Override default IterHandle prefix(String p) { return new IterHandle() { }; }
}
