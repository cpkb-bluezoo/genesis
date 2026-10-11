import java.io.Serializable;

/*
 * Regression test: a primitive passed to a parameter of a supertype of its wrapper other than
 * Object (Number, Comparable, Serializable) was not boxed: the unboxed value was left on the
 * stack where a reference was required (VerifyError, as in "JSONObject.numberToString(double)"
 * for "numberToString(Number)").
 */
public class PrimitiveToWrapperSupertypeParameterTest {

    final String seen;

    PrimitiveToWrapperSupertypeParameterTest(Number number) {
        this.seen = "ctor" + number;
    }

    static String number(Number value) {
        return "n" + value;
    }

    static String comparable(Comparable<?> value) {
        return "c" + value;
    }

    static String serializable(Serializable value) {
        return "s" + value;
    }

    public static void main(String[] args) {
        double d = 1.5;
        long l = 3L;
        int i = 2;
        char c = 'x';
        boolean z = true;
        String actual = number(d) + number(l) + number(i) + comparable(c) + comparable(z) + comparable(d)
            + serializable(i) + new PrimitiveToWrapperSupertypeParameterTest(l).seen;
        if (!"n1.5n3n2cxctruec1.5s2ctor3".equals(actual)) {
            throw new RuntimeException("FAIL: " + actual);
        }
        System.out.println("PASS");
    }
}
