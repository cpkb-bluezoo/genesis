package overridetest;

/*
 * Regression test: @Override checking used to fail with "does not override"
 * for an anonymous subclass whose overridden method is declared not on its
 * immediate superclass but on a *grandparent* class - and both are declared
 * *later* in this same file than the anonymous class's use of them. Walking
 * up the superclass chain needs to keep resolving each not-yet-visited
 * class's own "extends" clause and method declarations on demand. See
 * genesis history for details.
 */
public class ForwardSuperMethodOverrideTest {

    void run() {
        Middle m = new Middle() {
            @Override
            public void execute(Runnable task) {
                task.run();
            }
        };
        m.execute(() -> { });
    }

    private static class Middle extends Base {
    }

    private static class Base {
        public void execute(Runnable task) {
            task.run();
        }
    }
}
