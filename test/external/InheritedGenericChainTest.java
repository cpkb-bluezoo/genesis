import gchain.Tester;

/*
 * Regression test: "tester.parse(json).getObject().name()" for a field of type Tester<Example>,
 * where parse is declared by the generic superclass Base<T> of Tester<T> (a class file): the
 * result stayed in terms of Tester's own T (not Example), so the call on it was not found and
 * went to the wrong class (VerifyError).
 */
public class InheritedGenericChainTest {

    static class Example {
        String name() {
            return "spring";
        }
    }

    private Tester<Example> tester = new Tester<Example>(new Example());

    String chain() {
        return this.tester.parse("{}").getObject().name();
    }

    public static void main(String[] args) {
        String actual = new InheritedGenericChainTest().chain();
        if (!"spring".equals(actual)) {
            throw new RuntimeException("FAIL: " + actual);
        }
        System.out.println("PASS");
    }
}
