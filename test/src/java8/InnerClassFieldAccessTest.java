/** Inner class accesses protected field from outer superclass. */
public class InnerClassFieldAccessTest {

    static class Base {
        protected String endpoint = "ok";
    }

    static class Outer extends Base {
        void run() {
            new Inner().use();
        }

        class Inner {
            void use() {
                System.out.println(endpoint);
            }
        }
    }

    public static void main(String[] args) {
        new Outer().run();
        System.out.println("InnerClassFieldAccessTest OK");
    }
}
