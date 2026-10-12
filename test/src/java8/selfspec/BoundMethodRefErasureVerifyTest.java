import selfspec.SelfSpec;
import java.util.Collections;

/**
 * spec::apply must link the erased method (return SelfSpec), not Object.
 */
public class BoundMethodRefErasureVerifyTest {

    public static void main(String[] args) {
        SelfSpec<?> spec = null;
        try {
            Collections.singletonList("x").forEach(spec::apply);
        } catch (NullPointerException expected) {
            System.out.println("PASS");
            return;
        }
        throw new RuntimeException("expected NullPointerException from a null receiver");
    }
}
