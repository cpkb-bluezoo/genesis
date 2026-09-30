package primitivevarargsoverride;

/* Helper for PrimitiveVarargsOverrideVerifyTest: abstract varargs methods
 * whose element type is a primitive, declared in a DIFFERENT file from
 * the class overriding them. */
public abstract class Base {

    public abstract int sum(int... values);

    public abstract long total(int scale, long... values);

    public abstract int count(boolean... flags);

    public abstract String mixed(String prefix, double... values);

    public abstract int chars(char... cs);

    /* Array element type, and a plain (non-varargs) primitive array, for
     * contrast - both always worked. */
    public abstract int pieces(byte[]... parts);

    public abstract int plain(int[] values);
}
