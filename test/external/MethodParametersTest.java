import java.lang.reflect.Parameter;

/* Compiled with -parameters: reflection names the parameters of methods, constructors and abstract
 * methods (and the final modifier), instead of arg0, arg1... */
public class MethodParametersTest {
    public MethodParametersTest(long id, String label) {
    }

    public void update(String name, final int count) {
    }

    abstract static class Base {
        abstract void run(String first);
    }

    static String names(Parameter[] parameters) {
        StringBuilder out = new StringBuilder();
        for (Parameter parameter : parameters) {
            out.append(parameter.getName()).append(parameter.isNamePresent() ? "+" : "-").append(' ');
        }
        return out.toString();
    }

    public static void main(String[] args) throws Exception {
        String actual = names(MethodParametersTest.class.getMethod("update", String.class, int.class).getParameters())
            + names(MethodParametersTest.class.getConstructor(long.class, String.class).getParameters())
            + names(Base.class.getDeclaredMethod("run", String.class).getParameters());
        if (!"name+ count+ id+ label+ first+ ".equals(actual)) {
            throw new AssertionError(actual);
        }
        System.out.println("PASS");
    }
}
