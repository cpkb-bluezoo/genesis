/**
 * Regression: zero-arg generic varargs witness (ArgumentCaptor.captor / Mockito mock)
 * must infer T from the assignment target, not default to Object[].
 */
public class GenericVarargsWitnessVerifyTest {

    static final class Holder<T> {
        T value;
    }

    static <T> Holder<T> holder(T... witness) {
        return new Holder<>();
    }

    interface Marker {}

    static final class MarkerImpl implements Marker {}

    public static void main(String[] args) {
        Holder<Marker[]> h = holder();
        h.value = new Marker[] { new MarkerImpl() };
        if (!(h.value[0] instanceof MarkerImpl)) {
            throw new RuntimeException("bad witness inference");
        }
        System.out.println("PASS");
    }
}
