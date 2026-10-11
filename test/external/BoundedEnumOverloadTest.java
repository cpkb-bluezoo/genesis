import boundedenumlib.Status;

import static boundedenumlib.Assertions.assertThat;

/* An enum read from a class file is not a Delegate: assertThat(T extends Delegate) is not an
 * overload for it, the Comparable one is. */
public class BoundedEnumOverloadTest<V> {
    /* ...and so is a value of the caller's own unbounded type variable: the unbounded overload */
    boolean isPresent(V value) {
        return assertThat(value).isNotNull();
    }

    public static void main(String[] args) {
        if (!new BoundedEnumOverloadTest<String>().isPresent("x")) {
            throw new AssertionError("variable");
        }
        if (!assertThat(Status.UNAUTHORIZED).isEqualTo(Status.UNAUTHORIZED)) {
            throw new AssertionError("equal");
        }
        System.out.println("PASS");
    }
}
