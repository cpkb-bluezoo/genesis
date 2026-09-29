import java.util.ArrayList;
import java.util.List;
import java.nio.charset.StandardCharsets;

/*
 * Regression test: an enhanced for-loop whose declared loop-variable type
 * is itself an ARRAY type, iterating over a plain Iterable/Collection
 * (not an array) - e.g. "for (byte[] value : someListOfByteArrays)" -
 * matching gumdrop's own SearchResultEntry.getAttributeStringValues(),
 * which does exactly this over a List<byte[]> and then passes each
 * element to "new String(byte[], Charset)". Used to throw at class-
 * verification time:
 *
 *   java.lang.VerifyError: Bad type on operand stack
 *   Reason: Type 'java/lang/Object' (current frame, stack[N]) is not
 *   assignable to '[B'
 *
 * Root cause: AST_ENHANCED_FOR_STMT's Iterator-based codegen (used
 * whenever the ITERABLE isn't itself an array - a List<byte[]> is a
 * Collection, not an array, even though its ELEMENT type is an array)
 * only ever emitted a CHECKCAST down from Iterator.next()'s plain Object
 * result when the loop variable's declared type was a class type
 * (AST_CLASS_TYPE) - there was no handling at all for a loop variable
 * declared as an array type (AST_ARRAY_TYPE), so the checkcast was
 * silently skipped and the loop variable's stackmap-tracked type
 * defaulted to plain java.lang.Object. Any later use of the loop
 * variable that required its exact declared array type (e.g. passing it
 * to a byte[]-typed method/constructor parameter) then found a bare
 * Object on the operand stack instead of the expected array type.
 *
 * Fixed by adding an AST_ARRAY_TYPE branch that walks the array type
 * node's dimensions and innermost element type to build the correct JVM
 * array descriptor (e.g. "[B"), and using it both for the CHECKCAST
 * (JVMS 4.4.1: an array type's own class constant is its full
 * descriptor) and for the loop variable's stackmap entry.
 */
public class EnhancedForArrayElementOverCollectionVerifyTest {
    static List<String> toStrings(List<byte[]> values) {
        List<String> strings = new ArrayList<String>(values.size());
        for (byte[] value : values) {
            strings.add(new String(value, StandardCharsets.UTF_8));
        }
        return strings;
    }

    public static void main(String[] args) {
        List<byte[]> values = new ArrayList<byte[]>();
        values.add("hello".getBytes(StandardCharsets.UTF_8));
        values.add("world".getBytes(StandardCharsets.UTF_8));

        List<String> strings = toStrings(values);

        if (strings.size() != 2 || !"hello".equals(strings.get(0)) || !"world".equals(strings.get(1))) {
            throw new RuntimeException("expected [hello, world], got " + strings);
        }

        System.out.println("EnhancedForArrayElementOverCollectionVerifyTest passed!");
    }
}
