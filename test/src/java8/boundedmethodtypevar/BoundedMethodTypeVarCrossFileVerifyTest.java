package boundedmethodtypevar;

import java.util.HashMap;
import java.util.Map;

/* Regression test: a call, from ANOTHER file in the same batch, to a generic
 * METHOD whose type variable has a bound ("<A extends Annotation> A
 * annotation(Class<A>, Map)"). The method's descriptor erases A to its bound
 * (Annotation); the call site named Object, failing with NoSuchMethodError at
 * run time. Mirrors gumdrop's test AnnotationStubs.annotation(). */
public class BoundedMethodTypeVarCrossFileVerifyTest {
    @Deprecated
    static class Marked {
    }

    public static void main(String[] args) {
        Map<String, Object> v = new HashMap<String, Object>();
        Deprecated d = Maker.annotation(Deprecated.class, v);
        if (d != null) {
            throw new RuntimeException("expected null");
        }
        String s = Maker.pick("a", "b");
        if (!"b".equals(s)) {
            throw new RuntimeException("pick " + s);
        }
        System.out.println("PASS");
    }
}
