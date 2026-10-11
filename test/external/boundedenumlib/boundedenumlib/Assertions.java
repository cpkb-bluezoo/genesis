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

    public static class ObjectAssert<T> {
        private final T actual;

        ObjectAssert(T actual) {
            this.actual = actual;
        }

        public boolean isNotNull() {
            return actual != null;
        }
    }

    public static <T> ObjectAssert<T> assertThat(T actual) {
        return new ObjectAssert<T>(actual);
    }

    public static <T extends Delegate> T assertThat(T assertion) {
        return assertion;
    }

    public static <T extends Comparable<? super T>> ComparableAssert<T> assertThat(T actual) {
        return new ComparableAssert<T>(actual);
    }
}
