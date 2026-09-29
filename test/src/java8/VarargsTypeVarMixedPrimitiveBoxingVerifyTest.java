import java.util.Arrays;
import java.util.List;

/*
 * Regression test: calling a generic varargs method (e.g.
 * "<T> List<T> asList(T... a)", java.util.Arrays.asList) with a mix of
 * primitive-literal and reference-typed arguments (e.g.
 * "Arrays.asList(1, 2, 3, \"four\")", matching gumdrop's own AMQP
 * FieldTable test: "new FieldTable().put(\"list\", Arrays.asList(1, 2,
 * 3, \"four\"))") used to compile silently but generate a broken
 * AASTORE:
 *
 *   java.lang.VerifyError: Bad type on operand stack
 *   Reason: Type integer (current frame, stack[...]) is not
 *           assignable to 'java/lang/Object'
 *
 * Root cause: codegen_expr.c's varargs-array-population loop (the
 * method-call path) only boxed a primitive varargs argument when the
 * varargs parameter's element type resolved to TYPE_CLASS - missing a
 * bare, unresolved type variable (TYPE_TYPEVAR, exactly what "T" in
 * "T... a" resolves to at a non-generic-erased call site), which after
 * erasure is exactly as much a reference (Object) array as TYPE_CLASS
 * is. The array-creation logic just above the boxing check already
 * handled this correctly (falling through to its own "Default to
 * Object[]" branch for anything that isn't a recognized primitive
 * element type), but the boxing check itself never matched it, so an
 * int literal argument got AASTORE'd into the Object[] array unboxed.
 * Fixed by mirroring the array-creation logic's own primitive-vs-
 * reference test (type_kind_to_atype(elem_type->kind) >= 0) instead of
 * checking for TYPE_CLASS specifically.
 */
public class VarargsTypeVarMixedPrimitiveBoxingVerifyTest {
    public static void main(String[] args) {
        List<?> list = Arrays.asList(1, 2, 3, "four");
        if (!list.equals(Arrays.asList(1, 2, 3, "four"))) {
            throw new RuntimeException("expected [1, 2, 3, four] but got " + list);
        }
        System.out.println("VarargsTypeVarMixedPrimitiveBoxingVerifyTest passed!");
    }
}
