/*
 * Regression: a local anonymous class declared in a method of an abstract
 * class used to omit that anonymous class from the outer classfile's
 * InnerClasses attribute while the anonymous classfile itself listed the
 * outer as enclosing - IncompatibleClassChangeError at load time
 * (Spring Boot AbstractStructuredLoggingTests$1 from TypeReference<>() {}).
 */
abstract class AbstractLocalAnonymousInnerClassesVerifyTest {
    protected Runnable makeRunnable() {
        return new Runnable() {
            public void run() {
            }
        };
    }

    static final class Concrete extends AbstractLocalAnonymousInnerClassesVerifyTest {
    }

    public static void main(String[] args) throws Exception {
        Class.forName("AbstractLocalAnonymousInnerClassesVerifyTest");
        Class.forName("AbstractLocalAnonymousInnerClassesVerifyTest$1");
        Runnable r = new Concrete().makeRunnable();
        if (r == null) {
            throw new RuntimeException("null runnable");
        }
        System.out.println("PASS");
    }
}
