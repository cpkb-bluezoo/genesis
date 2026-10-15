/** An array-valued annotation on a local variable declaration must parse (JLS 9.7.1, 14.4). */
public class LocalArrayAnnotationTest {
    public static void main(String[] args) {
        try {
            @SuppressWarnings({"java:S2095", "all"})
            final String x = "a";
            @SuppressWarnings({"unchecked"}) final String y = x + "b";
            @SuppressWarnings(value = {"all"})
            String z = y + "c";
            @SuppressWarnings("all")
            final String w = z;
            if (!w.equals("abc")) {
                throw new AssertionError(w);
            }
        } catch (Exception e) {
            throw new AssertionError(e);
        }
        System.out.println("PASS");
    }
}
