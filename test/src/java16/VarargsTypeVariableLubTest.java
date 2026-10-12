import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Stream;

/**
 * Several arguments binding one method type variable ("Stream.of(a, b, c)") make it a type they
 * all fit, not the first one's: a lambda or method reference over the stream is typed by that,
 * so it must not cast the later elements to the first element's class.
 */
public class VarargsTypeVariableLubTest {

    enum Root { FIRST, LAST }

    static class Base { }
    static class Left extends Base { }
    static class Right extends Base { }

    record Props(Root root, Integer max, Boolean hashes) {
        boolean hasAny() {
            return Stream.of(root(), max(), hashes()).anyMatch(Objects::nonNull);
        }

        boolean hasAnyLambda() {
            return Stream.of(root(), max(), hashes()).anyMatch(x -> x != null);
        }
    }

    public static void main(String[] args) {
        if (!new Props(null, null, true).hasAny() || !new Props(null, null, true).hasAnyLambda()) {
            throw new AssertionError("a Boolean after a null Root");
        }
        if (new Props(null, null, null).hasAny()) {
            throw new AssertionError("all null");
        }
        long bases = Stream.of(new Left(), new Right()).map(b -> b.getClass().getSimpleName()).count();
        if (bases != 2) {
            throw new AssertionError("siblings");
        }
        List<Base> list = Arrays.asList(new Left(), new Right());
        if (list.size() != 2) {
            throw new AssertionError("asList");
        }
        System.out.println("PASS");
    }
}
