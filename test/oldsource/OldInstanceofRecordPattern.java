public class OldInstanceofRecordPattern {
    record Pt(int x, int y) {}

    static int sum(Object o) {
        if (o instanceof Pt(int a, int b)) {
            return a + b;
        }
        return 0;
    }
}
