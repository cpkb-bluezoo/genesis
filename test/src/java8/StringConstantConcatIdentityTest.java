import javax.xml.XMLConstants;

/**
 * A concatenation of compile-time constants is itself a constant: the string it names is the
 * interned one, so == against a literal is true, also for a static final field's value (which
 * the class's ConstantValue attribute and its static initializer must not disagree about).
 */
public class StringConstantConcatIdentityTest {

    static final String LOCAL = "3.0";
    static final String FIELD = "gumdrop/" + LOCAL;
    static final String CROSS = "gumdrop/" + Other.VERSION;
    static final String JDK = "x" + XMLConstants.XML_NS_PREFIX;

    static void check(boolean ok, String what) {
        if (!ok) {
            throw new AssertionError(what);
        }
    }

    public static void main(String[] args) {
        check(FIELD == "gumdrop/3.0", "field with a local constant");
        check(CROSS == "gumdrop/3.0", "field with another class's constant");
        check(JDK == "xxml", "field with a class-file constant");
        String inline = "a" + "b";
        check(inline == "ab", "two literals");
        String viaField = "g" + LOCAL;
        check(viaField == "g3.0", "literal and constant field");
        String viaOther = "g" + Other.VERSION + "!";
        check(viaOther == "g3.0!", "chain with another class's constant");
        String notConstant = args.length == 0 ? "z" : "y";
        check(("a" + notConstant) != "az", "a non-constant stays computed");
        check(("n" + 1).equals("n1"), "string and int");
        System.out.println("PASS");
    }
}

class Other {
    public static final String VERSION = "3.0";
}
