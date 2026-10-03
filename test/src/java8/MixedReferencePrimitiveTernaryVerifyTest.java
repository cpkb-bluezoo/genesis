/* Regression test: a conditional whose branches are a reference and a
 * primitive ("c == null ? "none" : arr.length"). The primitive is boxed and
 * the result is the lub of String and Integer, not String; the recorded
 * stack-map type was String and the boxed Integer reaching the merge failed
 * verification ("Type 'java/lang/Integer' is not assignable to
 * 'java/lang/String'"). Mirrors gumdrop's ServletEndToEndMoreTest. */
public class MixedReferencePrimitiveTernaryVerifyTest {
    static String show(Object[] certs) {
        StringBuilder sb = new StringBuilder();
        sb.append(certs == null ? "nocerts" : certs.length).append(';');
        sb.append(certs != null ? certs.length : "none").append(';');
        sb.append(certs == null ? -1 : certs.length);
        return sb.toString();
    }

    public static void main(String[] args) {
        if (!"nocerts;none;-1".equals(show(null))) {
            throw new RuntimeException(show(null));
        }
        if (!"2;2;2".equals(show(new Object[2]))) {
            throw new RuntimeException(show(new Object[2]));
        }
        Object o = args.length > 5 ? "s" : args.length;
        if (!Integer.valueOf(0).equals(o)) {
            throw new RuntimeException("object result " + o);
        }
        System.out.println("MixedReferencePrimitiveTernaryVerifyTest passed!");
    }
}
