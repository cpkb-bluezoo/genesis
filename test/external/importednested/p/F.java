package p;

public class F {
    static final class Fixture {
        Fixture(Object lb) { }

        static Fixture create() {
            return new Fixture(new Object());
        }

        int get() {
            return 7;
        }
    }
}
