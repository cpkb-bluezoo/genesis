import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

/* Regression test: generic calls typed by where they appear. A call passed as
 * an argument ("max(Comparator.comparingInt(c -> ...))") is typed against the
 * parameter it fills, so its lambda's parameter is known; a method reference
 * says what the receiver type variable is ("Comparator.comparing(String::length)
 * .compare(a, b)"). The lambda used to keep the parameter type it was first
 * given, before the call was typed against the parameter. */
public class ComparatorChainInferenceTest {
    record Item(String id) {
    }

    static Item longest(List<Item> items) {
        return items.stream().max(Comparator.comparingInt((c) -> c.id().length())).orElse(null);
    }

    static void sorted(List<Item> items) {
        items.sort(Comparator.comparing((Item i) -> i.id().length()).thenComparing(Item::id));
        items.sort(Comparator.comparingInt((i) -> -i.id().length()));
    }

    static int compareByLength(String a, String b) {
        return Comparator.comparing(String::length).compare(a, b);
    }

    static void take(Comparator<String> c, int[] out) {
        out[0] = c.compare("aa", "b");
    }

    public static void main(String[] args) {
        List<Item> items = new ArrayList<>(List.of(new Item("aaa"), new Item("b"), new Item("cc")));
        if (!"aaa".equals(longest(items).id())) {
            throw new RuntimeException("longest");
        }
        sorted(items);
        if (!"aaa".equals(items.get(0).id())) {
            throw new RuntimeException("sorted " + items);
        }
        if (compareByLength("a", "bb") >= 0 || compareByLength("bb", "a") <= 0) {
            throw new RuntimeException("compare");
        }
        int[] out = new int[1];
        take(Comparator.comparingInt((x) -> x.length()), out);
        if (out[0] <= 0) {
            throw new RuntimeException("take");
        }
        System.out.println("ComparatorChainInferenceTest passed!");
    }
}
