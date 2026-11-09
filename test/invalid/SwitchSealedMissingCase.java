public class SwitchSealedMissingCase {
    sealed interface S permits A, B {}
    record A(int x) implements S {}
    record B(int y) implements S {}

    static int f(S s) {
        return switch (s) {
            case A a -> 1;
        };
    }
}
