import java.time.Duration;

/*
 * Regression test: an enum constant's argument that a primitive-to-reference conversion had to
 * take to the constructor's parameter ("A(4, ...)" for "E(Object value)") was passed as the bare
 * int: VerifyError in the enum's static initializer.
 */
public class EnumConstantArgumentBoxingTest {

    private enum Setting {
        RETRY(4, "retry"),
        TIMEOUT(Duration.ofSeconds(120), "timeout"),
        ENABLED(true, "enabled"),
        WIDE(2, 1L),
        NAME("pool", "name");

        private final Object value;
        private final Object label;

        Setting(Object value, Object label) {
            this.value = value;
            this.label = label;
        }
    }

    public static void main(String[] args) {
        String actual = Setting.RETRY.value + " " + Setting.TIMEOUT.value + " " + Setting.ENABLED.value
            + " " + Setting.WIDE.value + Setting.WIDE.label + " " + Setting.NAME.value;
        if (!"4 PT2M true 21 pool".equals(actual)) {
            throw new RuntimeException("FAIL: " + actual);
        }
        System.out.println("EnumConstantArgumentBoxingTest passed!");
    }
}
