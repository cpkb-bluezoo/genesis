import java.util.ArrayList;
import java.util.List;

/* Regression test: two class-file overloads that both accept the argument,
 * (Object) and (Iterable<?>), scored the same for a List argument, so the one
 * the member table happened to yield first won - hasSameSizeAs(Object) instead
 * of the more specific hasSameSizeAs(Iterable) (JLS 15.12.2.5). */
public class MostSpecificOverloadClassFileTest {
    public static void main(String[] args) {
        List<String> list = new ArrayList<>();
        SizeCheck c = new SizeCheck();
        String r = c.hasSameSizeAs(list);
        if (!"Iterable".equals(r)) {
            System.out.println("FAILED: picked " + r);
            System.exit(1);
        }
        System.out.println("MostSpecificOverloadClassFileTest passed!");
    }
}
