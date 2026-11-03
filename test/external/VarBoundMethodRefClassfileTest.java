import java.util.Map;
import varboundrefslib.Factory;

/*
 * Regression test: a bound method reference on a "var" local whose type comes
 * from a javac-compiled class file resolves its method.
 */
public class VarBoundMethodRefClassfileTest {
    public static void main(String[] args) {
        var logger = Factory.create("GEN");
        Map.of("a", "1").forEach(logger::add);
        if (!"GEN a=1".equals(logger.seen.toString())) {
            throw new AssertionError(logger.seen);
        }
        System.out.println("VarBoundMethodRefClassfileTest passed!");
    }
}
