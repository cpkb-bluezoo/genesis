/* Library half of ExternalVarargsFlagTest: the concrete implementation,
 * plus a NATIVE varargs method (never called, only inspected by
 * reflection - a bodiless method like the abstract ones). */
public class VarargsImpl extends VarargsBase {

    public static VarargsImpl open() {
        return new VarargsImpl();
    }

    @Override
    public String command(String handler, String command, String... args) {
        return "S:" + command + ":" + args.length;
    }

    @Override
    public String command(String handler, String command, byte[]... args) {
        return "B:" + command + ":" + args.length;
    }

    @Override
    public int count(long scale, int... values) {
        return (int) scale * values.length;
    }

    @Override
    public String join(int n, String... parts) {
        return "J" + n + ":" + parts.length;
    }

    @Override
    public int size(Object... items) {
        return items.length;
    }

    public native int unused(int... values);
}
