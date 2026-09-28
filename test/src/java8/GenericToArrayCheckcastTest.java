import java.util.ArrayList;
import java.util.List;

/*
 * Regression test: the result of a generic method call whose declared
 * return type is T[] (e.g. Collection<T>.toArray(T[] a)) used to produce
 * a class file that failed JVM bytecode verification when passed
 * somewhere requiring the concrete array type:
 *
 *   java.lang.VerifyError: Bad type on operand stack
 *   Reason: Type '[Ljava/lang/Object;' is not assignable to
 *   '[Ljava/lang/String;'
 *
 * T[] erases to Object[] (or the type variable's bound array) at the JVM
 * level, exactly like a bare type-variable return (e.g. Supplier<T>.get())
 * erases to Object - and codegen_expr.c already inserted a checkcast to
 * narrow that case back to the statically-known type. But that logic only
 * checked expr->sem_type->kind == TYPE_CLASS, so it never fired for an
 * array-typed generic return, and no checkcast was ever emitted for
 * List<String>.toArray(new String[0]) and similar - leaving the erased
 * Object[] on the stack wherever the caller expected String[].
 * See genesis history for details (search "Same idea, for a method that
 * returns T[]" in codegen_expr.c).
 */
public class GenericToArrayCheckcastTest {
    static class Holder {
        final String[] names;
        Holder(String[] names) { this.names = names; }
    }

    public static void main(String[] args) {
        List<String> list = new ArrayList<>();
        list.add("a");
        list.add("b");

        // Constructor argument position - the shape that originally
        // surfaced this bug (gumdrop's MemoryPath constructor call).
        Holder h = new Holder(list.toArray(new String[0]));
        if (h.names.length != 2 || !"a".equals(h.names[0]) || !"b".equals(h.names[1])) {
            throw new RuntimeException("constructor-argument toArray failed");
        }

        // Plain assignment position.
        String[] direct = list.toArray(new String[0]);
        if (direct.length != 2) {
            throw new RuntimeException("assignment toArray failed");
        }

        System.out.println("All tests passed!");
    }
}
