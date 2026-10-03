import innerctorlib.Endpoint;
import java.util.concurrent.CountDownLatch;

/* Regression test: "outer.new Inner(args)" where Inner comes from a CLASS
 * FILE. The call site's constructor descriptor must be the constructor's own
 * (minus the leading outer-instance parameter), not one rebuilt from the
 * argument expressions: a null argument was typed Object ("<init>(Endpoint,
 * Object)": NoSuchMethodError), and an int literal passed for a long
 * parameter was typed int. Mirrors gumdrop's OtlpEndpointsExtraTest:
 * "e.new OtlpConnectionHandler(null)". */
public class InnerCtorFromClassfileTest {
    public static void main(String[] args) {
        Endpoint e = new Endpoint();
        Endpoint.Handler none = e.new Handler(null);
        if (none.latch != null) {
            throw new RuntimeException("null latch");
        }
        CountDownLatch l = new CountDownLatch(3);
        Endpoint.Handler h = e.new Handler(l);
        if (h.latch.getCount() != 3) {
            throw new RuntimeException("latch");
        }
        Endpoint.Wide w = e.new Wide(5, "five");
        if (w.id != 5L || !"five".equals(w.label)) {
            throw new RuntimeException("wide");
        }
        System.out.println("InnerCtorFromClassfileTest passed!");
    }
}
