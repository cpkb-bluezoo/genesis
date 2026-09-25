import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

/*
 * String concatenation with boxed operands, including values that come out
 * of generic calls (list.get, Function.apply, a Box<T> getter or field).
 * A wrapper object is an Object: it must be appended as one, not as the
 * primitive it wraps, and null must print as "null".
 */
public class StringConcatBoxedTest {

    static class Box<T> {
        T value;

        Box(T value) {
            this.value = value;
        }

        T get() {
            return value;
        }
    }

    static void check(String actual, String expected) {
        if (!expected.equals(actual)) {
            System.out.println("FAILED: expected " + expected + " but got " + actual);
            System.exit(1);
        }
    }

    public static void main(String[] args) {
        List<Integer> ints = new ArrayList<Integer>();
        ints.add(3);
        check("v=" + ints.get(0), "v=3");

        Function<Integer, Integer> inc = new Function<Integer, Integer>() {
            public Integer apply(Integer x) {
                return x + 1;
            }
        };
        check("v=" + inc.apply(41), "v=42");

        Box<Integer> box = new Box<Integer>(7);
        check("v=" + box.get(), "v=7");
        check("v=" + box.value, "v=7");
        check(box.get() + "!", "7!");

        Integer boxed = 5;
        check("v=" + boxed, "v=5");
        Integer none = null;
        check("v=" + none, "v=null");
        check("v=" + new Box<Integer>(null).get(), "v=null");

        Long l = 9L;
        Double d = 2.5;
        Boolean b = Boolean.TRUE;
        Character c = 'x';
        check("v=" + l + d + b + c, "v=92.5truex");

        List<Character> chars = new ArrayList<Character>();
        chars.add('q');
        check("v=" + chars.get(0), "v=q");
        List<Long> longs = new ArrayList<Long>();
        longs.add(4L);
        check("v=" + longs.get(0), "v=4");
        List<Double> doubles = new ArrayList<Double>();
        doubles.add(1.5);
        check("v=" + doubles.get(0), "v=1.5");

        int i = 1;
        char ch = 'z';
        check("v=" + i + ch + 2L, "v=1z2");
        check(1 + 2 + "v" + 1 + 2, "3v12");
        String s = "a";
        s = s + ints.get(0);
        check(s, "a3");

        System.out.println("StringConcatBoxedTest passed!");
    }
}
