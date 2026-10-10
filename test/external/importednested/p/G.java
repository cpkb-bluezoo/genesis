package p;

public class G {
    static final class Fixture {
        int other() {
            return 1;
        }
    }

    int x() {
        return new Fixture().other();
    }
}
