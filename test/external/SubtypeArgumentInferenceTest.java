import static providerlib.Providers.first;

import providerlib.StringContext;

/* A type variable fixed by an argument that is a subtype of the parameter's class
 * ("first(Provider<T>)" given a StringContext, which is a Provider<Result<String>> through
 * Context<String>) is what that class says: a Result<String>, not the String argument of the
 * subtype. */
public class SubtypeArgumentInferenceTest {
    public static void main(String[] args) {
        StringContext context = StringContext.of("abc");
        String described = first(context).describe();
        int length = first(context).value().length();
        if (!"result of abc".equals(described) || length != 3) {
            throw new AssertionError(described + length);
        }
        System.out.println("PASS");
    }
}
