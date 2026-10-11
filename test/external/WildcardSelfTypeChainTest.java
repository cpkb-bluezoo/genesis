import selftypelib.ObjectCheck;

/* A fluent self-typed class-file API ("SELF extends Check<SELF, A>") reached through a
 * wildcard receiver ("Check<?, Object>"): every call of the chain still returns a type the
 * next call can be made on, and a lambda at its end is typed. */
public class WildcardSelfTypeChainTest {
    public static void main(String[] args) {
        StringBuilder log = new StringBuilder();
        new ObjectCheck<String>("hello", log).extracting("w")
            .has("a", 1)
            .has("b", 2)
            .has("c", 3)
            .extracting("q")
            .satisfies(String.class, (text) -> log.append(text.length()));
        if (!"extract w;a=1;b=2;c=3;extract q;5".equals(log.toString())) {
            throw new AssertionError(log.toString());
        }
        System.out.println("PASS");
    }
}
