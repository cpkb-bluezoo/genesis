public class SwitchIntNoDefault {
    static int f(int i) {
        return switch (i) {
            case 1 -> 10;
            case 2 -> 20;
        };
    }
}
