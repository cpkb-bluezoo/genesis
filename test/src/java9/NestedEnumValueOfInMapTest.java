import java.util.Optional;

public class NestedEnumValueOfInMapTest {
    public enum Setting {
        ON, OFF
    }

    static Optional<String> lookup(String key) {
        return Optional.of("on");
    }

    static Setting parseSetting(String setting) {
        return lookup(setting)
                .map(String::toUpperCase)
                .map(Setting::valueOf)
                .orElse(Setting.OFF);
    }

    public static void main(String[] args) {
        if (parseSetting("x") != Setting.ON) {
            throw new AssertionError();
        }
        System.out.println("NestedEnumValueOfInMapTest passed!");
    }
}
