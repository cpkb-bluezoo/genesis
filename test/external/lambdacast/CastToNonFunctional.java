public class CastToNonFunctional {
    void m() {
        Object o = (String) () -> 1;
    }
}
