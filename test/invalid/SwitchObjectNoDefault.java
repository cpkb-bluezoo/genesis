public class SwitchObjectNoDefault {
    static int f(Object o) {
        return switch (o) {
            case String s -> 1;
            case Integer i -> 2;
        };
    }
}
