/* Regression test: Class.getDeclaredClasses() follows the order of the
 * InnerClasses attribute, which javac writes with the member classes in
 * reverse declaration order. Frameworks that walk member classes (Spring's
 * configuration class parser) see their beans in that order, so a different
 * order changes which bean is created first. */
public class DeclaredClassesOrderTest {
    static class First {
    }

    static class Second {
    }

    interface Third {
    }

    enum Fourth {
        X
    }

    public static void main(String[] args) {
        StringBuilder order = new StringBuilder();
        for (Class<?> c : DeclaredClassesOrderTest.class.getDeclaredClasses()) {
            order.append(c.getSimpleName()).append(' ');
        }
        String expected = "Fourth Third Second First ";
        if (!expected.equals(order.toString())) {
            System.out.println("FAILED: expected " + expected + "got " + order);
            System.exit(1);
        }
        System.out.println("DeclaredClassesOrderTest passed!");
    }
}
