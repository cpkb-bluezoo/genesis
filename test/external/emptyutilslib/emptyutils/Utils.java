package emptyutils;

public final class Utils {

    private Utils() {
    }

    public static boolean isEmpty(Object[] array) {
        return array == null || array.length == 0;
    }

    public static boolean isEmpty(Object object) {
        return object == null || "".equals(object);
    }
}
