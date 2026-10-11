import java.util.List;
import java.util.function.Function;

/* Regression test: an element of a List<? extends Var> is a Var - a member of it
 * can be called on the result of get() (javax.lang.model's
 * getParameters().get(0).asType() in Spring Boot's TypeElementMembers). */
public class WildcardElementMemberAccessTest {
    interface Var {
        String name();
    }

    interface Exe {
        List<? extends Var> params();
    }

    static String first(Exe e) {
        return e.params().get(0).name();
    }

    static String viaLambda(Exe e) {
        Function<Exe, String> f = (c) -> c.params().get(0).name();
        return f.apply(e);
    }

    public static void main(String[] args) {
        Var v = () -> "p";
        Exe e = () -> List.of(v);
        if (!"p".equals(first(e)) || !"p".equals(viaLambda(e))) {
            throw new RuntimeException("wildcard element");
        }
        System.out.println("WildcardElementMemberAccessTest passed!");
    }
}
