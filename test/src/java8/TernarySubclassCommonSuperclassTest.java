import java.io.Serializable;

/*
 * A conditional whose operands are two sibling subclasses of a class that
 * itself implements an interface both share has the class as its type (the
 * lub, JLS 15.25, 4.10.4), not the interface.
 */
public class TernarySubclassCommonSuperclassTest {
    static class Bag implements Serializable {
        public String toString() { return "bag"; }
    }
    static class Left extends Bag implements Serializable {}
    static class Right extends Bag implements Serializable {}

    static Right fetch(Object id) { return new Right(); }

    public static void main(String[] args) {
        Object id = args.length == 0 ? "x" : null;
        Left pending = new Left();
        Bag dao = id != null ? fetch(id) : pending;
        Bag other = id == null ? fetch(id) : pending;
        if (!dao.toString().equals("bag") || other != pending) {
            throw new RuntimeException("wrong operand selected");
        }
        System.out.println("TernarySubclassCommonSuperclassTest passed!");
    }
}
