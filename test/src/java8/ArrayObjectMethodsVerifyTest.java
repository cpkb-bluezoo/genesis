/* Regression test: java.lang.Object methods called on an ARRAY receiver
 * (getClass, hashCode, equals, toString). Every such call must name
 * java/lang/Object as the owner - genesis named the array's element class
 * for an object array ("Number.getClass") and no class at all for a
 * primitive array, both rejected by the verifier. (clone() is the one
 * method arrays override; it was already handled.) */
public class ArrayObjectMethodsVerifyTest {
    public static void main(String[] args) {
        Number[] n = new Number[] { 1, 2 };
        int[] p = new int[3];
        String[][] s = new String[1][1];

        if (n.getClass() != Number[].class) {
            throw new RuntimeException("Number[] getClass");
        }
        if (p.getClass() != int[].class) {
            throw new RuntimeException("int[] getClass");
        }
        if (!s.getClass().getSimpleName().equals("String[][]")) {
            throw new RuntimeException("String[][] getClass");
        }
        if (n.hashCode() != System.identityHashCode(n)) {
            throw new RuntimeException("hashCode");
        }
        if (!n.equals(n) || n.equals(new Number[] { 1, 2 }) || p.equals(null)) {
            throw new RuntimeException("equals");
        }
        if (!p.toString().startsWith("[I@")) {
            throw new RuntimeException("toString " + p);
        }
        System.out.println("ArrayObjectMethodsVerifyTest passed!");
    }
}
