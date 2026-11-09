public class SwitchGuardedPatternNotTotal {
    static int f(String s) {
        return switch (s) {
            case String t when t.isEmpty() -> 0;
        };
    }
}
