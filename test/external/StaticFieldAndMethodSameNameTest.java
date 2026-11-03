import staticfieldmethodmid.Mid;

/*
 * Regression test: a static field of a javac-compiled superclass that also
 * declares an instance method of the same name is found by its simple name
 * through a source subclass of a source subclass.
 */
public class StaticFieldAndMethodSameNameTest extends Mid {
    String run() {
        try {
            log.error("failed");
        } catch (IllegalStateException expected) {
            return expected.getMessage();
        }
        return null;
    }

    public static void main(String[] args) {
        if (!"failed".equals(new StaticFieldAndMethodSameNameTest().run())) {
            throw new AssertionError();
        }
        System.out.println("StaticFieldAndMethodSameNameTest passed!");
    }
}
