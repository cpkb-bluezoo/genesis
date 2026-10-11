/* Regression test: an interface may redeclare the public methods of Object with
 * @Override (Spring Boot's AnsiElement), and a method called main that is not a
 * launcher's main(String[]) is just a method (SpringApplicationBuilder.main(Class)). */
interface Element {
    @Override
    String toString();

    @Override
    int hashCode();

    @Override
    boolean equals(Object o);
}

public class InterfaceRedeclaresObjectMethodsTest {
    private Class<?> main;

    InterfaceRedeclaresObjectMethodsTest main(Class<?> mainClass) {
        this.main = mainClass;
        return this;
    }

    public static void main(String[] args) {
        Element e = new Element() {
            @Override
            public String toString() {
                return "e";
            }
        };
        InterfaceRedeclaresObjectMethodsTest t = new InterfaceRedeclaresObjectMethodsTest().main(String.class);
        if (!"e".equals(e.toString()) || t.main != String.class) {
            throw new RuntimeException("redeclared");
        }
        System.out.println("InterfaceRedeclaresObjectMethodsTest passed!");
    }
}
