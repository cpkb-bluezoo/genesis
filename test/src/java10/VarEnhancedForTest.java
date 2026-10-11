import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/*
 * Regression test: "for (var x : things)" gave x the type of the declaration's "var" (an int for
 * an array of references, an Object for an Iterable), failing verification when x was used.
 */
public class VarEnhancedForTest {

    public static void main(String[] args) {
        StringBuilder out = new StringBuilder();
        String[] names = { "ab", "c" };
        for (var name : names) {
            out.append(name.length());
        }
        for (var number : new int[] { 1, 2 }) {
            out.append(number + 1);
        }
        for (var row : new int[][] { { 1, 2 }, { 3 } }) {
            out.append(row.length);
        }
        List<Map.Entry<String, Integer>> entries = new ArrayList<>();
        entries.add(Map.entry("k", 7));
        for (var entry : entries) {
            out.append(entry.getKey()).append(entry.getValue() + 1);
        }
        List<int[]> arrays = new ArrayList<>();
        arrays.add(new int[3]);
        for (var array : arrays) {
            out.append(array.length);
        }
        if (!"212321k83".equals(out.toString())) {
            throw new RuntimeException("FAIL: " + out);
        }
        System.out.println("VarEnhancedForTest passed!");
    }
}
