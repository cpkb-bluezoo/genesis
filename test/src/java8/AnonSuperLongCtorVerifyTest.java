/**
 * Regression: new Super(2500) { } must forward int literal to super(long), not (int).
 */
public class AnonSuperLongCtorVerifyTest {

    static class Base {
        final long value;

        Base(long value) {
            this.value = value;
        }
    }

    public static void main(String[] args) {
        Base b = new Base(2500) {};
        if (b.value != 2500L) {
            throw new RuntimeException("expected 2500, got " + b.value);
        }
        System.out.println("PASS");
    }
}
