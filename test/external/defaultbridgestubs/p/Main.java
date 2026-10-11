package p;
// A default method of Iter with a narrower return type than Source.prefix: Main needs the bridge Iter carries.
public class Main implements Iter {
    public static void main(String[] a) { Source s = new Main(); System.out.println(s.prefix("x") != null); }
}
