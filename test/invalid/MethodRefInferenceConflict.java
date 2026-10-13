/* Z is fixed to Foo by the unbound method reference Foo::getName, and to Integer by the
 * argument 1: no instantiation fits both (OpenJDK's MethodReference68). */
class MethodRefInferenceConflict {
    interface F<X> {
        String m(X x);
    }

    static class Foo {
        String getName() {
            return "";
        }
    }

    @SuppressWarnings("unchecked")
    <Z> void g(F<Z> fz, Z... zs) {
    }

    void test() {
        g(Foo::getName, 1);
    }
}
