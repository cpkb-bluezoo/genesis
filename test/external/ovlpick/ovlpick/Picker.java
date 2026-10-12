package ovlpick;

public final class Picker {
    private Picker() {
    }

    public interface Delegate {
        String id();
    }

    public interface Marker {
        String id();
    }

    public static String invokeMethod(Object target, String name, Object... args) {
        return "object";
    }

    public static String invokeMethod(Class<?> targetClass, String name, Object... args) {
        return "class";
    }

    public static <T> String arrays(T[] actual) {
        return "1d";
    }

    public static <T> String arrays(T[][] actual) {
        return "2d";
    }

    public static class Real implements Delegate {
        @Override
        public String id() {
            return "d";
        }
    }

    public static <T> String pick(T actual) {
        return "object";
    }

    public static <T extends Delegate> String pick(T actual) {
        return "delegate";
    }
}
