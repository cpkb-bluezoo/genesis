import nestedouter.lib.Registration;
import nestedouter.lib.Shape;

/**
 * A nested type, loaded from a class file, that extends its own outer type:
 * the members it inherits from the outer type must be found whichever of
 * the two the compiler happens to load first.
 */
public class NestedExtendsOuterTest {

    /* Name the outer types before the nested ones */
    static Registration outerFirst;
    static Shape shapeFirst;

    static class Impl implements Registration.Dynamic {
        public String describe(String... parts) {
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < parts.length; i++) {
                if (i > 0) {
                    sb.append(',');
                }
                sb.append(parts[i]);
            }
            return sb.toString();
        }

        public void flag(boolean on) {
        }
    }

    public static void main(String[] args) {
        Registration.Dynamic dynamic = new Impl();
        /* describe() is declared by Registration, inherited by Dynamic */
        String described = dynamic.describe("a", "b");
        if (!"a,b".equals(described)) {
            throw new AssertionError("describe: " + described);
        }

        Shape.Circle circle = new Shape.Circle();
        /* name() is declared by Shape, inherited by Circle */
        String name = circle.name();
        if (!"shape".equals(name) || circle.corners() != 0) {
            throw new AssertionError("name: " + name);
        }

        System.out.println("NestedExtendsOuterTest passed");
    }
}
