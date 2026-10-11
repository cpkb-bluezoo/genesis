import qtgflib.Base;

/* "Use.this.actual" from an inner class: actual is a protected field of a generic superclass in
 * another package, declared with the type variable A (an Object in the class file) and a
 * CharSequence here. Its descriptor is the declared one, a cast follows, and the read goes
 * through an accessor of Use, the only class that may read it. */
public class QualifiedThisGenericFieldTest extends Base<QualifiedThisGenericFieldTest, CharSequence> {

    QualifiedThisGenericFieldTest(CharSequence value) {
        super(value);
    }

    class Inner {
        CharSequence qualified() {
            CharSequence value = QualifiedThisGenericFieldTest.this.actual;
            return value;
        }

        int length() {
            return QualifiedThisGenericFieldTest.this.actual.length();
        }
    }

    public static void main(String[] args) {
        QualifiedThisGenericFieldTest outer = new QualifiedThisGenericFieldTest("abc");
        Inner inner = outer.new Inner();
        if (!"abc".equals(inner.qualified().toString()) || inner.length() != 3) {
            throw new AssertionError("read");
        }
        System.out.println("PASS");
    }
}
