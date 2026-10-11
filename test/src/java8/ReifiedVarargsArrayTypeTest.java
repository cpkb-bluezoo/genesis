/*
 * Regression test: for "<T> T create(T... reified)" called as "StringBuilder b = create();" the
 * array holding the (no) arguments is a StringBuilder[], T being what the call is assigned to
 * (Mockito's mock(T... reified) reads the class from that array): it was an Object[].
 */
public class ReifiedVarargsArrayTypeTest {

    @SafeVarargs
    @SuppressWarnings("unchecked")
    static <T> T create(T... reified) {
        try {
            return (T) reified.getClass().getComponentType().getDeclaredConstructor().newInstance();
        } catch (ReflectiveOperationException ex) {
            throw new IllegalStateException(ex);
        }
    }

    private final StringBuilder field = create();

    public static void main(String[] args) {
        StringBuilder local = create();
        if (local == null || new ReifiedVarargsArrayTypeTest().field == null) {
            throw new RuntimeException("FAIL");
        }
        System.out.println("ReifiedVarargsArrayTypeTest passed!");
    }
}
