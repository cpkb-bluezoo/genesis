package constlib;

public class Constants {
    public static final String NAME = "n";
    public static final int COUNT = 5;
    public static final long BIG = 7L;
    public static final boolean FLAG = true;
    public static final char LETTER = 'c';
    public static final double RATIO = 1.5;
    public static final float SMALL = 0.25f;

    static {
        if (NAME != null) {
            throw new IllegalStateException("Constants must not be initialized");
        }
    }
}
