/* Regression test: array-typed elements of an Object[] initializer
 * ("new Object[] { new byte[16] }"). An array's expression kind reads as its
 * ELEMENT's kind, so the primitive-boxing added for "new Object[] { delay,
 * attempt }" also fired for the array and called Byte.valueOf(byte[]) -
 * "Bad type on operand stack". Mirrors gumdrop's ClusterRecordingMemberTest:
 * method.invoke(cluster, new Object[] { new byte[16] }). */
public class ObjectArrayElementArrayVerifyTest {
    static Object[] build(byte[] given) {
        long[] longs = new long[2];
        return new Object[] { new byte[16], new long[3], given, longs, new int[] { 1, 2 },
                new char[1], 5 };
    }

    public static void main(String[] args) {
        Object[] a = build(new byte[] { 9 });
        if (a.length != 7 || !(a[0] instanceof byte[]) || ((byte[]) a[0]).length != 16
                || !(a[1] instanceof long[]) || ((byte[]) a[2])[0] != 9 || !(a[3] instanceof long[])
                || ((int[]) a[4])[1] != 2 || !(a[5] instanceof char[]) || !Integer.valueOf(5).equals(a[6])) {
            throw new RuntimeException("wrong elements");
        }
        System.out.println("ObjectArrayElementArrayVerifyTest passed!");
    }
}
