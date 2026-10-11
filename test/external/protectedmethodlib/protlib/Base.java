package protlib;

public abstract class Base {

    protected void fail(String message, Object... args) {
        throw new IllegalStateException(String.format(message, args));
    }

    protected String describe(int value, long wide, double more) {
        return "d" + value + wide + more;
    }

    protected static String stat(String value) {
        return "s" + value;
    }
}
