/*
 * Regression test: indexing directly into the result of a generic
 * `List<T>.get(int)` call where `T` is itself an array type (e.g.
 * `List<byte[]>`) - `messages.get(i)[0]` - exactly gumdrop's own
 * TcpEndpointApplicationDataTest shape (a List<byte[]> of buffered
 * message bytes, each one indexed directly off get()'s own result
 * inside a loop) - used to throw:
 *
 *   java.lang.VerifyError: Bad type on operand stack in baload
 *   Invalid type: 'java/lang/Object' (current frame, stack[N])
 *
 * Root cause: codegen_method_call()'s existing type-variable-erasure
 * checkcast logic (codegen_expr.c) already covered two shapes - a
 * method declared to return a bare type variable T, resolved at the
 * call site to a concrete class (e.g. Supplier<String>.get()), and a
 * method declared to return T[], resolved to a concrete array type
 * (e.g. Collection<T>.toArray(T[] a)) - but not a THIRD shape: a method
 * declared to return a bare type variable T (method_sym->type->kind ==
 * TYPE_TYPEVAR, not TYPE_ARRAY) whose call-site type ARGUMENT itself
 * happens to be an array (e.g. T = byte[] for a List<byte[]>). Without
 * the checkcast this case needed, the erased `Object` result from
 * `get(int)` was left on the stack wherever it was used, rejected the
 * moment something needing the real array type (here, a `baload`
 * for `[0]`) consumed it directly.
 *
 * A second, related gap surfaced alongside it: even with the checkcast
 * added, the SAME call's own stackmap-tracking "push return value" logic
 * still switched on method_sym->type->kind alone (still TYPE_TYPEVAR,
 * unaffected by any call-site substitution) - silently falling through
 * to a plain "assume int" default. Harmless as long as the value is
 * consumed immediately, before any frame gets recorded (as in a minimal,
 * branch-free repro) - but wrong, tracking a stale int where a real
 * array reference belongs, the instant the same call result needs to
 * survive across a loop/if/try boundary first (exactly this test's own
 * shape, and gumdrop's). Fixed by preferring the call site's own
 * resolved type (expr->sem_type) over the bare type variable when
 * deciding what to push for stackmap-tracking purposes too.
 */
import java.util.ArrayList;
import java.util.List;

public class GenericListArrayElementGetVerifyTest {
    public static void main(String[] args) {
        List<byte[]> messages = new ArrayList<>();
        for (int i = 0; i < 5; i++) {
            messages.add(new byte[]{(byte) i, (byte) (i + 100)});
        }
        for (int i = 0; i < 5; i++) {
            byte first = messages.get(i)[0];
            byte last = messages.get(i)[1];
            if (first != (byte) i) {
                throw new AssertionError("first mismatch at " + i + ": " + first);
            }
            if (last != (byte) (i + 100)) {
                throw new AssertionError("last mismatch at " + i + ": " + last);
            }
        }
        System.out.println("GenericListArrayElementGetVerifyTest passed!");
    }
}
