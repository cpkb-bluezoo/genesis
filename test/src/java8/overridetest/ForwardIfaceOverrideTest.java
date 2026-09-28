package overridetest;

/*
 * Regression test: @Override checking used to fail with "does not override"
 * for an anonymous subclass of an abstract class whose "implements" clause
 * hadn't been resolved yet, because that abstract class is declared *later*
 * in this same file than its use here. See genesis history for details.
 */
public class ForwardIfaceOverrideTest {

    interface Greeter {
        void greet(String name);
    }

    void run() {
        AbstractGreeter g = new AbstractGreeter() {
            @Override
            public void greet(String name) {
            }
        };
        g.greet("world");
    }

    private abstract static class AbstractGreeter implements Greeter {
    }
}
