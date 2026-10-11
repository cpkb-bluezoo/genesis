import selftypelib.Fluent;

/* The result of a call that is a method variable bounded by a variable of its class ("<T extends B>
 * T named(..)" with "B extends Fluent<B>"), reached through a wildcard, is something the next
 * call can be made on. */
public class MethodVariableBoundByClassVariableTest {
    public static void main(String[] args) {
        String built = Fluent.create().named("x").build();
        if (!"built x".equals(built)) {
            throw new AssertionError(built);
        }
        System.out.println("PASS");
    }
}
