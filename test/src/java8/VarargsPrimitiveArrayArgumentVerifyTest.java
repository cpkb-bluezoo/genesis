/* Regression test: a primitive array as the single argument for an Object...
 * parameter. byte[] is an Object (one vararg element) but NOT an Object[],
 * so it must be wrapped - genesis passed it as the varargs array itself:
 * "Type '[B' is not assignable to '[Ljava/lang/Object;'". An Object[] or
 * String[] is passed through unchanged. Mirrors gumdrop's
 * RespCodecEdgeTest: encoder.encode("GET", new byte[] { 'k', '\r' }). */
public class VarargsPrimitiveArrayArgumentVerifyTest {
    static int count(String name, Object... args) {
        return args.length;
    }

    static Object first(Object... args) {
        return args[0];
    }

    public static void main(String[] args) {
        byte[] b = { 1, 2 };
        if (count("x", b) != 1 || count("x", new byte[] { 3 }) != 1 || count("x", new int[3]) != 1) {
            throw new RuntimeException("primitive array must be one element");
        }
        if (first(b) != b) {
            throw new RuntimeException("element identity");
        }
        if (count("x", new Object[] { 1, 2, 3 }) != 3 || count("x", new String[] { "a", "b" }) != 2) {
            throw new RuntimeException("reference arrays pass through");
        }
        if (count("x", (Object) new Object[] { 1, 2 }) != 1) {
            throw new RuntimeException("cast to Object wraps");
        }
        System.out.println("VarargsPrimitiveArrayArgumentVerifyTest passed!");
    }
}
