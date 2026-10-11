package wcres;

public class Picker {

    public interface Marker {
    }

    public static class Holder<T> {
        private final T value;

        public Holder(T value) {
            this.value = value;
        }

        public T get() {
            return this.value;
        }
    }

    public static <T extends Marker> String pick(T marker) {
        return "marker";
    }

    public static String pick(Iterable<?> value) {
        return "iterable";
    }

    public static String pick(Runnable value) {
        return "runnable";
    }

    public static <T extends Comparable<? super T>> String pick(T value) {
        return "comparable";
    }

    public static <T> String pick(T value) {
        return "generic " + value;
    }
}
