import fieldchain.lib.Base;

/**
 * Reading a field of a field that the class inherits from a superclass
 * loaded from a class file ("kind.extension", where kind is declared by
 * Base): the expression must have the type of the last field, whether it
 * is assigned, concatenated or reached through "this".
 */
public class InheritedFieldChainTest extends Base {

    String assigned() {
        String extension = kind.extension;
        return extension;
    }

    String concatenated() {
        return "x" + kind.extension;
    }

    String concatenatedLong() {
        return "x" + kind.size;
    }

    String publicField() {
        String text = holder.text;
        return text + holder.count;
    }

    String staticField() {
        return "x" + SHARED.text;
    }

    String throughThis() {
        String extension = this.kind.extension;
        return extension + this.holder.text;
    }

    static void check(String what, String expected, String actual) {
        if (!expected.equals(actual)) {
            throw new AssertionError(what + ": expected " + expected + ", got " + actual);
        }
    }

    public static void main(String[] args) {
        InheritedFieldChainTest t = new InheritedFieldChainTest();
        check("assigned", ".java", t.assigned());
        check("concatenated", "x.java", t.concatenated());
        check("concatenatedLong", "x7", t.concatenatedLong());
        check("publicField", "held3", t.publicField());
        check("staticField", "xheld", t.staticField());
        check("throughThis", ".javaheld", t.throughThis());
        System.out.println("InheritedFieldChainTest passed");
    }
}
