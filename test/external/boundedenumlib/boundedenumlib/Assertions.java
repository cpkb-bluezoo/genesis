package boundedenumlib;

public final class Assertions {
    private Assertions() {
    }

    public static class ComparableAssert<T> {
        private final T actual;

        ComparableAssert(T actual) {
            this.actual = actual;
        }

        public boolean isEqualTo(Object expected) {
            return actual == expected;
        }
    }

    public static <T extends Delegate> T assertThat(T assertion) {
        return assertion;
    }

    public static <T extends Comparable<? super T>> ComparableAssert<T> assertThat(T actual) {
        return new ComparableAssert<T>(actual);
    }
}
