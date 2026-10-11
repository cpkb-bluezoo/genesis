import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/* Regression test: the declared type of a variable being initialized was also
 * used to infer the type variable of a generic call that is only the RECEIVER
 * of another call - "String s = id(list).toString()" cast id's result to String
 * and failed with a ClassCastException. */
public class GenericReceiverIgnoresTargetTypeTest {
    static <T> T id(T t) {
        return t;
    }

    public static void main(String[] args) {
        List<String> l = new ArrayList<>(List.of("a", "b"));
        String viaId = id(l).toString();
        String viaStream = l.stream().map(x -> x + "!").collect(Collectors.toList()).toString();
        String chained = id(Integer.valueOf(12)).toString().trim();
        if (!"[a, b]".equals(viaId) || !"[a!, b!]".equals(viaStream) || !"12".equals(chained)) {
            throw new RuntimeException(viaId + viaStream + chained);
        }
        System.out.println("GenericReceiverIgnoresTargetTypeTest passed!");
    }
}
