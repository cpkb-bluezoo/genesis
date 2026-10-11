package p;

/* A lone value for an array-typed element of an annotation type compiled in the same batch
 * (@Cond(container = X.class)) is a one-element array in the class file, as for a type
 * read from a class file. */
@Cond(container = String.class, names = "one")
public class SingleValueForArrayElementTest {

    @Cond(String.class)
    @Retention1
    static class Marked {
    }

    @java.lang.annotation.Retention(java.lang.annotation.RetentionPolicy.RUNTIME)
    @interface Retention1 {
    }

    public static void main(String[] args) {
        Cond cond = SingleValueForArrayElementTest.class.getAnnotation(Cond.class);
        if (cond == null || cond.container().length != 1 || cond.container()[0] != String.class
                || cond.names().length != 1 || !"one".equals(cond.names()[0])) {
            throw new AssertionError("container");
        }
        Cond marked = Marked.class.getAnnotation(Cond.class);
        if (marked == null || marked.value().length != 1 || marked.value()[0] != String.class) {
            throw new AssertionError("value");
        }
        System.out.println("PASS");
    }
}
