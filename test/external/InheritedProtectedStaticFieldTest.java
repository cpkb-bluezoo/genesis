import protectedstaticfieldlib.BaseStats;

/*
 * Regression test: a protected static field inherited from a superclass
 * that is only available as a javac-compiled .class file on the classpath
 * is visible, by its simple name, in the subclass body.
 */
public class InheritedProtectedStaticFieldTest extends BaseStats {
    public InheritedProtectedStaticFieldTest() {
        super("TYPE");
    }

    String report() {
        try {
            super.getStatsType();
            throw new Exception("boom");
        } catch (Exception ex) {
            try {
                log.error("Error logging " + getStatsType());
            } catch (IllegalStateException expected) {
                return expected.getMessage();
            }
            return null;
        }
    }

    public static void main(String[] args) {
        if (!"Error logging TYPE".equals(new InheritedProtectedStaticFieldTest().report())) {
            throw new AssertionError();
        }
        System.out.println("InheritedProtectedStaticFieldTest passed!");
    }
}
