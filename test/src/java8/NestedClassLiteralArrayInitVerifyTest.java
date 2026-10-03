/* Regression test: class literals of nested classes (named by simple name)
 * as ELEMENTS OF AN ARRAY INITIALIZER. "Class<?> c = S.class;" resolved to
 * U$S, but "Class<?>[] cs = { S.class, P.class };" emitted the bare name
 * "S" (NoClassDefFoundError). Mirrors gumdrop's ContextScanClassTest
 * ("Class<?>[] ... = { A.class, B.class }"-style lists of nested test
 * classes). */
public class NestedClassLiteralArrayInitVerifyTest {
    static class S {
    }

    static class P {
    }

    interface Q {
    }

    public static void main(String[] args) {
        Class<?>[] cs = { S.class, P.class, Q.class };
        Class<?>[] ds = new Class<?>[] { P.class, S.class };
        String want = "NestedClassLiteralArrayInitVerifyTest$";
        if (!(want + "S").equals(cs[0].getName()) || !(want + "P").equals(cs[1].getName())
                || !(want + "Q").equals(cs[2].getName()) || !(want + "P").equals(ds[0].getName())) {
            throw new RuntimeException(cs[0].getName() + " " + cs[1].getName());
        }
        Object[] mixed = { S.class, "x" };
        if (mixed[0] != S.class) {
            throw new RuntimeException("mixed");
        }
        System.out.println("NestedClassLiteralArrayInitVerifyTest passed!");
    }
}
