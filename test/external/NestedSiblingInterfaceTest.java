import nestedsiblinglib.Responses;

/* A nested interface that extends its sibling ("Body extends Headers<Body>"), when the outer
 * class is first loaded because the sibling is named as "Responses.Headers<?>": Body still has
 * Headers' methods. */
public class NestedSiblingInterfaceTest {
    private Responses.Headers<?> unused;

    public static void main(String[] args) {
        String built = Responses.status(200).header("a").contentType("t").build();
        if (!"status 200 header a type t".equals(built)) {
            throw new AssertionError(built);
        }
        System.out.println("PASS");
    }
}
