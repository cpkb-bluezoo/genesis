public class UnnamedVariableTest {
    public static void main(String[] args) {
        testLocalDiscard();
        testMultipleUnnamed();
        testCatchUnnamed();
        testForUnnamed();
        testLambdaUnnamed();
        System.out.println("All unnamed variable tests passed!");
    }

    static int sideEffect;

    static void testLocalDiscard() {
        sideEffect = 0;
        int _ = bump();
        int _ = bump();
        if (sideEffect != 2) {
            throw new RuntimeException("expected 2 side effects, got " + sideEffect);
        }
    }

    static int bump() {
        return ++sideEffect;
    }

    static void testMultipleUnnamed() {
        String _ = "a";
        String _ = "b";
        int x = 42;
        if (x != 42) {
            throw new RuntimeException("named local broken");
        }
    }

    static void testCatchUnnamed() {
        try {
            throw new RuntimeException("boom");
        } catch (RuntimeException _) {
            /* discarded */
        }
    }

    static void testForUnnamed() {
        int[] nums = {1, 2, 3};
        int count = 0;
        for (int _ : nums) {
            count++;
        }
        if (count != 3) {
            throw new RuntimeException("for-each count=" + count);
        }
    }

    static void testLambdaUnnamed() {
        java.util.function.Function<String, Integer> f = _ -> 7;
        if (f.apply("ignored") != 7) {
            throw new RuntimeException("lambda unnamed failed");
        }
    }
}
