public class CircularCtorHelper {
    final CircularCtorDependencyTest owner;

    CircularCtorHelper(CircularCtorDependencyTest owner) {
        this.owner = owner;
    }
}
