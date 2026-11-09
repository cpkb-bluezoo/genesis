public class SwitchRecordPatternMissingCase {
    sealed interface S permits A, B {}
    record A(int x) implements S {}
    record B(int y) implements S {}
    record Box(S first, S second) {}

    static int f(Box b) {
        return switch (b) {
            case Box(A x, A y) -> 1;
            case Box(A x, B y) -> 2;
            case Box(B x, A y) -> 3;
        };
    }
}
