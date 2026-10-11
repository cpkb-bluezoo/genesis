import itersrc.MapSrc;
import itersrc.Src;

/*
 * Regression test: "MapSrc" (a class file, not generic) implements IterSrc, which extends
 * Iterable<String>; its Iterable-ness with those arguments was not found, because a class
 * without a Signature attribute had its interfaces left out. "pick(Iterable<Src>)" was then
 * taken for it (ClassCastException at run time) instead of "pick(Src...)".
 */
public class ClassFileInterfaceChainOverloadTest {

    static String pick(Src... sources) {
        return "varargs";
    }

    static String pick(Iterable<Src> sources) {
        return "iterable";
    }

    public static void main(String[] args) {
        String actual = pick(new MapSrc());
        if (!"varargs".equals(actual)) {
            throw new RuntimeException("FAIL: " + actual);
        }
        System.out.println("PASS");
    }
}
