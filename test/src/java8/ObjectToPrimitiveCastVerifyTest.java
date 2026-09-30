import java.lang.reflect.Method;

/*
 * "(int) someObjectTypedExpr" (e.g. the Object returned by
 * java.lang.reflect.Method.invoke()) is an UNBOXING cast per JLS 5.5:
 * checkcast to the target primitive's own wrapper class, then unbox -
 * never a primitive-to-primitive conversion opcode. AST_CAST_EXPR's
 * "primitive cast" codegen (codegen_expr.c) only ever switched on a
 * PRIMITIVE source_kind (int/long/float/double/byte/short/char/boolean);
 * a reference-typed (TYPE_CLASS) source fell through with no case at
 * all, emitting NO conversion whatsoever and leaving the raw reference
 * on the stack: VerifyError "Bad type on operand stack ... Object ...
 * is not assignable to integer".
 *
 * Confirmed against gumdrop's own
 * H3ClientStreamTest.testExtractStatusReturns200(): "int result = (int)
 * m.invoke(null, ...);".
 */
public class ObjectToPrimitiveCastVerifyTest {
    static int compute() {
        return 200;
    }

    public static void main(String[] args) throws Exception {
        Method m = ObjectToPrimitiveCastVerifyTest.class.getDeclaredMethod("compute");
        m.setAccessible(true);
        int result = (int) m.invoke(null);
        if (result != 200) {
            throw new RuntimeException("expected 200, got " + result);
        }
        System.out.println("ObjectToPrimitiveCastVerifyTest passed!");
    }
}
