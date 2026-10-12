/**
 * An overload applicable only by unboxing ("isEqualTo(int)" for an Integer argument) loses to an
 * inherited one applicable without it ("isEqualTo(Object)"): the strict phase of overload
 * resolution looks at every method of the receiver, inherited or not, before the loose one does.
 * Choosing the int one failed with a NullPointerException for a null Integer.
 */
public class BoxedArgPrefersInheritedObjectOverloadTest {

    static class Assert<S> {
        String isEqualTo(Object expected) {
            return "Object";
        }
    }

    static class IntAssert extends Assert<IntAssert> {
        String isEqualTo(int expected) {
            return "int";
        }

        String isEqualTo(long expected) {
            return "long";
        }
    }

    interface Api {
        Integer value();
    }

    static IntAssert assertThat() {
        return new IntAssert();
    }

    static Integer nothing() {
        return null;
    }

    static void expect(String want, String got) {
        if (!want.equals(got)) {
            throw new AssertionError("expected " + want + " but chose " + got);
        }
    }

    public static void main(String[] args) {
        IntAssert a = new IntAssert();
        Integer boxed = null;
        Api api = () -> null;
        expect("Object", a.isEqualTo(boxed));
        expect("Object", a.isEqualTo(api.value()));
        expect("Object", a.isEqualTo(Integer.valueOf(3)));
        expect("int", a.isEqualTo(3));
        expect("long", a.isEqualTo(3L));
        Long boxedLong = null;
        expect("Object", a.isEqualTo(boxedLong));
        expect("Object", assertThat().isEqualTo(nothing()));
        System.out.println("PASS");
    }
}
