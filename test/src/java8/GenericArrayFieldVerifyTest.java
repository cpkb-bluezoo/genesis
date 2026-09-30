import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;

/*
 * A generic field ("T value") erases to its bound (Object, if unbounded)
 * in the class file - reading it through a use site where T is
 * instantiated to a more specific type needs a checkcast back to that
 * type, exactly like a method returning a type variable.
 * checkcast_generic_field() (codegen_expr.c) already handled this for a
 * CLASS-typed instantiation (e.g. FieldValue<String>) but returned early,
 * emitting no checkcast at all, whenever the resolved type was an ARRAY
 * (e.g. FieldValue<byte[]>) - its own guard only accepted
 * expr->sem_type->kind == TYPE_CLASS. The erased field then stayed typed
 * as Object, rejected the moment it was used somewhere requiring the
 * real array type: VerifyError "Bad type on operand stack ... Object
 * ... is not assignable to '[B'".
 *
 * Confirmed against gumdrop's own ProtobufParserTest, whose generic
 * "FieldValue<byte[]>.value" hits exactly this via "new
 * String(handler.bytes.get(0).value, ...)".
 */
public class GenericArrayFieldVerifyTest {
    static class FieldValue<T> {
        final int fieldNumber;
        final T value;

        FieldValue(int fieldNumber, T value) {
            this.fieldNumber = fieldNumber;
            this.value = value;
        }
    }

    public static void main(String[] args) throws UnsupportedEncodingException {
        List<FieldValue<byte[]>> bytes = new ArrayList<>();
        bytes.add(new FieldValue<byte[]>(1, "hello".getBytes("UTF-8")));
        String result = new String(bytes.get(0).value, "UTF-8");
        if (!"hello".equals(result)) {
            throw new RuntimeException("expected hello, got " + result);
        }
        System.out.println("GenericArrayFieldVerifyTest passed!");
    }
}
