/* The valid counterparts of an inference conflict: an unbound method reference fixes the type
 * variable to its receiver type, and arguments of that type (or a subtype) are fine, as is a
 * static method reference, which fixes nothing about the receiver. */
public class MethodRefInferenceTest {
    interface F<X> {
        String m(X x);
    }

    static class Foo {
        String getName() {
            return "foo";
        }
    }

    static class SubFoo extends Foo {
    }

    static String describe(Object o) {
        return "object:" + o;
    }

    @SuppressWarnings("unchecked")
    <Z> String g(F<Z> fz, Z... zs) {
        StringBuilder out = new StringBuilder();
        for (Z z : zs) {
            out.append(fz.m(z)).append(',');
        }
        return out.toString();
    }

    public static void main(String[] args) {
        MethodRefInferenceTest t = new MethodRefInferenceTest();
        String none = t.g(Foo::getName);
        String same = t.g(Foo::getName, new Foo(), new Foo());
        String sub = t.g(Foo::getName, new SubFoo());
        String statics = t.g(MethodRefInferenceTest::describe, 1, "x");
        if (!none.isEmpty() || !same.equals("foo,foo,") || !sub.equals("foo,") ||
            !statics.equals("object:1,object:x,")) {
            System.out.println("FAILED: " + none + " " + same + " " + sub + " " + statics);
            System.exit(1);
        }
        System.out.println("MethodRefInferenceTest passed!");
    }
}
