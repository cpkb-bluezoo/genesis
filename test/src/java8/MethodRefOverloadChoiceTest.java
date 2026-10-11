import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;

/* Regression test: the overload a method reference names is the one whose
 * parameters suit what the functional interface passes, not the first with the
 * right number of them: String::valueOf as a Function<Object, String> was
 * valueOf(int) and failed to link ("Object is not convertible to int"). */
public class MethodRefOverloadChoiceTest {
    static String pick(Object o) {
        return "object";
    }

    static String pick(int i) {
        return "int";
    }

    static String pick(String s) {
        return "string";
    }

    String describe(Object o) {
        return "o";
    }

    String describe(long l) {
        return "l";
    }

    public static void main(String[] args) {
        Function<Object, String> f = String::valueOf;
        Function<Integer, String> g = String::valueOf;
        Function<char[], String> h = String::valueOf;
        Function<Object, String> p1 = MethodRefOverloadChoiceTest::pick;
        Function<Integer, String> p2 = MethodRefOverloadChoiceTest::pick;
        Function<String, String> p3 = MethodRefOverloadChoiceTest::pick;
        BiFunction<MethodRefOverloadChoiceTest, Object, String> d1 = MethodRefOverloadChoiceTest::describe;
        BiFunction<MethodRefOverloadChoiceTest, Long, String> d2 = MethodRefOverloadChoiceTest::describe;
        StringBuilder sb = new StringBuilder();
        Consumer<String> c = sb::append;
        Consumer<Integer> i = sb::append;
        Consumer<Object> o = sb::append;
        c.accept("hi");
        i.accept(7);
        o.accept(1.5);
        MethodRefOverloadChoiceTest t = new MethodRefOverloadChoiceTest();
        String got = f.apply(3) + g.apply(4) + h.apply(new char[] {'x'}) + "," + p1.apply("s") + p2.apply(1)
                + p3.apply("s") + "," + d1.apply(t, "x") + d2.apply(t, 5L) + "," + sb;
        if (!"34x,objectobjectstring,oo,hi71.5".equals(got)) {
            throw new RuntimeException(got);
        }
        System.out.println("MethodRefOverloadChoiceTest passed!");
    }
}
