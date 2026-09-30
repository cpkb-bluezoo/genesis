package varargsarrayelement;

/* Helper for VarargsArrayElementVerifyTest. */
public class Impl implements Session {

    @Override
    public String command(String handler, String command, String... args) {
        return "S:" + command + ":" + args.length;
    }

    @Override
    public String command(String handler, String command, byte[]... args) {
        int total = 0;
        for (byte[] a : args) {
            total += a.length;
        }
        return "B:" + command + ":" + args.length + "/" + total;
    }

    @Override
    public int pieces(byte[]... parts) {
        return parts.length;
    }

    public static String join(char sep, int[]... rows) {
        StringBuilder sb = new StringBuilder();
        for (int[] row : rows) {
            if (sb.length() > 0) {
                sb.append(sep);
            }
            sb.append(row.length);
        }
        return sb.toString();
    }
}
