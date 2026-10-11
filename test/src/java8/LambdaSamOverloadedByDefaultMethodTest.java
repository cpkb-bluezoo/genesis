/* Regression test: a functional interface whose default method has the same
 * name as its abstract one (Spring Boot's AggregateElementBinder.bind) - the
 * lambda was matched against the default method's parameters, so none of its
 * own parameters resolved. */
interface OverloadedSam {
    default Object bind(String name, Object target) {
        return bind(name, target, null);
    }

    Object bind(String name, Object target, String source);
}

public class LambdaSamOverloadedByDefaultMethodTest {
    static Object use(OverloadedSam b) {
        return b.bind("a", "b");
    }

    public static void main(String[] args) {
        OverloadedSam b = (name, target, source) -> name + target + source;
        if (!"abnull".equals(use(b))) {
            throw new RuntimeException(String.valueOf(use(b)));
        }
        if (!"x|y".equals(b.bind("x", "?", "y").toString().replace("?", "|").replace("x|", "x|"))) {
            throw new RuntimeException("explicit");
        }
        if (!"a|null".equals(use((n, t, s) -> n + "|" + s))) {
            throw new RuntimeException("argument");
        }
        System.out.println("LambdaSamOverloadedByDefaultMethodTest passed!");
    }
}
