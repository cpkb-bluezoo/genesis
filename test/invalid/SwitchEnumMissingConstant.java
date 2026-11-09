public class SwitchEnumMissingConstant {
    enum Colour { RED, GREEN, BLUE }

    static int f(Colour c) {
        return switch (c) {
            case RED -> 1;
            case GREEN -> 2;
        };
    }
}
