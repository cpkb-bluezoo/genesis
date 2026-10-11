/*
 * Regression test: a class nested in an interface was listed twice in the interface's
 * NestMembers attribute. A tool that rewrites the class (a Mockito inline mock of the
 * interface) writes it without the repeat, which the JVM rejects as a change of the nest.
 */
public interface InterfaceNestMembersUniqueTest<T> {

    void customize(T value);

    class Builder<T> {
    }

    static void main(String[] args) {
        // the host itself and Builder
        int members = InterfaceNestMembersUniqueTest.class.getNestMembers().length;
        if (members != 2) {
            throw new RuntimeException("FAIL: " + members + " nest members");
        }
        System.out.println("InterfaceNestMembersUniqueTest passed!");
    }
}
