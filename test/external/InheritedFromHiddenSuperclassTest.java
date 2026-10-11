import hiddenlib.Shown;

/*
 * Regression test: a method inherited from a package-private superclass of another package
 * ("Shown" extends the hidden "Hidden", which declares create/st) was called by its declaring
 * class, which this class cannot access (IllegalAccessError). A call by simple name is
 * qualified by the class it is made in (JLS 13.1).
 */
public class InheritedFromHiddenSuperclassTest extends Shown<String> {

    String go() {
        return create("x") + st("y") + open();
    }

    public static void main(String[] args) {
        String actual = new InheritedFromHiddenSuperclassTest().go();
        if (!"cxtyo".equals(actual)) {
            throw new RuntimeException("FAIL: " + actual);
        }
        System.out.println("PASS");
    }
}
