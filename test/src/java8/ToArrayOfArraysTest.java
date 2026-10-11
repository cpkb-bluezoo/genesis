import java.util.ArrayList;
import java.util.List;

/* Regression test: List<long[]>.toArray(new long[0][]) - the "T[]" parameter takes
 * an array of arrays, T being long[] (gumdrop's QuicConnection, once a generic
 * method's own T stopped being confused with the list's). */
public class ToArrayOfArraysTest {
    static long[][] ranges(List<long[]> list) {
        return list.toArray(new long[0][]);
    }

    static long[][] sized(List<long[]> list) {
        return list.toArray(new long[list.size()][]);
    }

    public static void main(String[] args) {
        List<long[]> list = new ArrayList<>();
        list.add(new long[] {1, 2});
        list.add(new long[] {3});
        long[][] a = ranges(list);
        long[][] b = sized(list);
        String[] s = new ArrayList<>(List.of("x")).toArray(new String[0]);
        if (a.length != 2 || a[0][1] != 2 || b.length != 2 || b[1][0] != 3 || !"x".equals(s[0])) {
            throw new RuntimeException("toArray");
        }
        System.out.println("ToArrayOfArraysTest passed!");
    }
}
