import objectreturnlib.Exposable;
import objectreturnlib.Stubbing;

/* A method an interface that is not generic declares to return Object returns Object, whatever
 * type argument the interface it is reached through gives its own supertype: "Exposable" is an
 * Endpoint<Integer>, but its getController() is not an Integer. */
public class ObjectReturnOfNonGenericInterfaceTest {
    public static void main(String[] args) {
        Exposable endpoint = new Exposable() {
            public Integer operation() {
                return 1;
            }

            public Object getController() {
                return "controller";
            }
        };
        Object controller = "controller";
        Stubbing<Object> stubbing = Stubbing.given(endpoint.getController()).willReturn(controller);
        if (!"controller".equals(stubbing.value())) {
            throw new AssertionError(stubbing.value());
        }
        System.out.println("PASS");
    }
}
