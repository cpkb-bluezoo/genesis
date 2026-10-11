package p;

/* A lambda passed to forEach of a class compiled in the same batch that implements
 * Iterable<Its.Nested>: the lambda's parameter is the nested type, whichever file comes first. */
public class IterableOfNestedTypeTest {
    private final Mappings target = new Mappings();

    void addAll(Mappings other) {
        other.forEach((mapping) -> this.target.add(mapping.getExtension(), mapping.getType()));
    }

    public static void main(String[] args) {
        IterableOfNestedTypeTest test = new IterableOfNestedTypeTest();
        Mappings source = new Mappings();
        source.add("txt", "text/plain");
        test.addAll(source);
        int count = 0;
        for (Mappings.Mapping mapping : test.target) {
            count += "text/plain".equals(mapping.getType()) ? 1 : 0;
        }
        if (count != 1) {
            throw new AssertionError(count);
        }
        System.out.println("PASS");
    }
}
