public class CastToArray {
    void m() {
        Object o = (Runnable[]) () -> { };
    }
}
