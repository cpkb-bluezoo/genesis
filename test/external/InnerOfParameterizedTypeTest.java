import iplib.Configurer;

/* The parameter type of authorize() in the REAL-javac-compiled library is the signature
 * Consumer<Configurer<Http>.Registry>: a member class of a parameterized type. The lambda's
 * parameter is the binary class Configurer$Registry (not Configurer/Registry). */
public class InnerOfParameterizedTypeTest {
    static String seen;

    public static void main(String[] args) {
        new Configurer.Http().authorize(r -> seen = r.name());
        if (!"registry".equals(seen)) {
            throw new AssertionError(seen);
        }
        System.out.println("PASS");
    }
}
