import boundedenumlib.Status;

import static boundedenumlib.Assertions.assertThat;

/* An enum read from a class file is not a Delegate: assertThat(T extends Delegate) is not an
 * overload for it, the Comparable one is. */
public class BoundedEnumOverloadTest {
    public static void main(String[] args) {
        if (!assertThat(Status.UNAUTHORIZED).isEqualTo(Status.UNAUTHORIZED)) {
            throw new AssertionError("equal");
        }
        System.out.println("PASS");
    }
}
