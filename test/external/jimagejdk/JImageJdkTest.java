import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

/* Compiled against a JDK that has only lib/modules (no jmods directory): types of
 * java.base and of another module must resolve from the runtime image. */
public class JImageJdkTest {
    public static void main(String[] args) {
        List<String> names = new ArrayList<>();
        names.add("jimage");
        Logger.getLogger("jimage").fine("not shown");
        System.out.println(String.join(",", names));
    }
}
