import java.util.Arrays;
import java.util.List;

/* Regression test: a lambda that uses a local variable or parameter named
 * like an instance field of the class is using the local. The capture scan
 * took the name for the field and made the lambda capture this, so a lambda in
 * a static method was generated as an instance one (VerifyError "not
 * assignable to ..."), and one in an instance method captured the wrong thing. */
public class LambdaCapturesLocalShadowingFieldTest {
    String s = "field";
    int n = 100;

    static boolean inStatic(Object o, List<String> names) {
        String s = (String) o;
        return names.stream().anyMatch(x -> x.equals(s));
    }

    long inInstance(List<Integer> values) {
        int n = 2;
        return values.stream().filter(v -> v > n).count();
    }

    long viaParameter(List<Integer> values, int n) {
        return values.stream().filter(v -> v > n).count();
    }

    long viaField(List<Integer> values) {
        return values.stream().filter(v -> v > this.n).count();
    }

    long lambdaParameterShadows(List<Integer> values) {
        return values.stream().map(n -> n + 1).filter(n -> n > 3).count();
    }

    public static void main(String[] args) {
        if (!inStatic("b", Arrays.asList("a", "b")) || inStatic("z", Arrays.asList("a", "b"))) {
            throw new RuntimeException("static");
        }
        LambdaCapturesLocalShadowingFieldTest t = new LambdaCapturesLocalShadowingFieldTest();
        List<Integer> v = Arrays.asList(1, 2, 3, 4, 200);
        if (t.inInstance(v) != 3) {
            throw new RuntimeException("instance " + t.inInstance(v));
        }
        if (t.viaParameter(v, 3) != 2) {
            throw new RuntimeException("parameter");
        }
        if (t.viaField(v) != 1) {
            throw new RuntimeException("field");
        }
        if (t.lambdaParameterShadows(v) != 3) {
            throw new RuntimeException("lambda parameter " + t.lambdaParameterShadows(v));
        }
        System.out.println("LambdaCapturesLocalShadowingFieldTest passed!");
    }
}
