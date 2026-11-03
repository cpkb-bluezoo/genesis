import java.util.Optional;

public class NestedEnumValueOfMethodRefTest {
    static class Outer {
        enum Setting { ON, OFF }
    }

    public static void main(String[] args) {
        Outer.Setting setting = Optional.of("on")
                .map(String::toUpperCase)
                .map(Outer.Setting::valueOf)
                .orElse(Outer.Setting.OFF);
        if (setting != Outer.Setting.ON) {
            throw new AssertionError(setting);
        }
        System.out.println("NestedEnumValueOfMethodRefTest passed!");
    }
}
