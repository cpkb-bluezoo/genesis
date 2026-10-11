import nulconst.Marker;

/*
 * Regression test: a String constant containing a NUL ("\u0000") was written to its field's
 * ConstantValue attribute cut short at the NUL, as "". A class compiled against that class
 * file inlined the empty string, and no longer matched what the library itself compared with
 * (gumdrop's web fragment "others" marker).
 */
public class EmbeddedNulConstantValueTest {

    public static void main(String[] args) {
        String nul = Marker.NUL;
        String mixed = Marker.MIXED;
        if (nul.length() != 1 || nul.charAt(0) != 0) {
            throw new RuntimeException("FAIL NUL: " + nul.length());
        }
        if (mixed.length() != 3 || mixed.charAt(1) != 0) {
            throw new RuntimeException("FAIL MIXED: " + mixed.length());
        }
        System.out.println("PASS");
    }
}
