import java.lang.reflect.Parameter;
import java.lang.reflect.RecordComponent;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/*
 * Regression test: what reflection reads of a record. The Record attribute (getRecordComponents()
 * was null), the generic types of the component, field, accessor and canonical constructor (the
 * Signature attributes), and the names of the canonical constructor's parameters (mandated
 * MethodParameters - without which Spring Boot could not bind a record from properties).
 */
public class RecordReflectionTest {

    record Inner(String a) {
    }

    record Implicit(List<Inner> list, Map<String, Integer> map, int count) {
    }

    record Compact(List<String> names, int size) {
        Compact {
            Objects.requireNonNull(names);
        }
    }

    record Explicit(List<String> names, int size) {
        Explicit(List<String> names, int size) {
            this.names = names;
            this.size = size;
        }
    }

    static String describe(Class<?> type) throws Exception {
        StringBuilder out = new StringBuilder();
        for (RecordComponent component : type.getRecordComponents()) {
            out.append(component.getName()).append('=').append(component.getGenericType()).append(' ');
        }
        out.append(type.getDeclaredConstructors()[0].getGenericParameterTypes().length).append(' ');
        for (Parameter parameter : type.getDeclaredConstructors()[0].getParameters()) {
            out.append(parameter.getName()).append(parameter.isNamePresent() ? '+' : '-');
        }
        out.append(' ').append(type.getDeclaredMethod(type.getRecordComponents()[0].getName()).getGenericReturnType());
        return out.toString();
    }

    public static void main(String[] args) throws Exception {
        String actual = describe(Implicit.class) + "|" + describe(Compact.class) + "|" + describe(Explicit.class);
        String expected = "list=java.util.List<RecordReflectionTest$Inner> map=java.util.Map<java.lang.String, java.lang.Integer>"
            + " count=int 3 list+map+count+ java.util.List<RecordReflectionTest$Inner>"
            + "|names=java.util.List<java.lang.String> size=int 2 names+size+ java.util.List<java.lang.String>"
            + "|names=java.util.List<java.lang.String> size=int 2 names+size+ java.util.List<java.lang.String>";
        if (!expected.equals(actual)) {
            throw new RuntimeException("FAIL: " + actual);
        }
        System.out.println("RecordReflectionTest passed!");
    }
}
