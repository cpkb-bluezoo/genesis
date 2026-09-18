public class FlexibleCtorTest {
    final int value;

    FlexibleCtorTest(int x) {
        if (x < 0) {
            throw new IllegalArgumentException("negative");
        }
        int doubled = x * 2;
        super();
        this.value = doubled;
    }

    FlexibleCtorTest(String s) {
        int n = s.length();
        this(n);
    }

    public static void main(String[] args) {
        FlexibleCtorTest a = new FlexibleCtorTest(3);
        if (a.value != 6) {
            throw new RuntimeException("expected 6, got " + a.value);
        }
        FlexibleCtorTest b = new FlexibleCtorTest("hi");
        if (b.value != 4) {
            throw new RuntimeException("expected 4, got " + b.value);
        }
        try {
            new FlexibleCtorTest(-1);
            throw new RuntimeException("should have thrown");
        } catch (IllegalArgumentException e) {
            /* expected */
        }
        System.out.println("All flexible constructor tests passed!");
    }
}
